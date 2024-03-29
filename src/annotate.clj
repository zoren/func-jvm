(ns annotate
  (:require
   [clojure.string]
   [unify :refer [mk-type-var unify normalize specialize generalize]]
   )
  )

(defn try-get-class [class-name]
  (try
    (Class/forName class-name)
    (catch java.lang.ClassNotFoundException _ nil)))

(defn try-get-constructor [class-obj parameter-types]
  (try
    (when class-obj (.getConstructor class-obj (into-array Class parameter-types)))
    (catch java.lang.NoSuchMethodException _ nil)))

;;https://docs.oracle.com/javase/specs/jls/se7/html/jls-5.html#jls-5.1.7
(def primitive-type->wrapper
  {Boolean/TYPE Boolean
   Byte/TYPE Byte
   Short/TYPE Short
   Character/TYPE Character
   Integer/TYPE Integer
   Long/TYPE Long
   Float/TYPE Float
   Double/TYPE Double
   })

(defn wrap-primitive-types [t]
  (if (and (vector? t) (= (count t) 1))
    [(primitive-type->wrapper (first t) (first t))]
    (primitive-type->wrapper t t)))

(defn get-arity-methods [class-obj method-name number-of-arguments]
  (filter
   (fn [method]
     (and (= (.getName method) method-name)
          (not (.isBridge method))
          (= (.getParameterCount method) number-of-arguments)
          (every?
           #(not (instance? java.lang.reflect.GenericArrayType (.getParameterizedType %)))
           (.getParameters method))))
   (.getMethods class-obj)))

(defn specialize-type [mapping]
  (fn type-> [t]
    (cond
      (instance? java.lang.reflect.TypeVariable t)
      (mapping t)

      (instance? java.lang.reflect.ParameterizedType t)
      (into [(.getRawType ^java.lang.reflect.ParameterizedType t)]
            (map type-> (.getActualTypeArguments ^java.lang.reflect.ParameterizedType t)))

      (class? t)
      [t]

      :else
      (throw (ex-info "specialize-method: unsupported type" {:t t :type (type t) :class? (class? t)})))))

(def java-generic-type->type
  (specialize-type #(throw (ex-info "type var not expected" {:tv %}))))

(defn specialize-method [method]
  (let [m (into {} (map (fn [tp] [tp (mk-type-var 0)]) (.getTypeParameters method)))
        type-> (specialize-type m)]
    [(map #(type-> (.getParameterizedType %)) (.getParameters method))
     (type-> (.getGenericReturnType method))]))

(defn get-type-parameters [method]
  (let [type-parameters (.getTypeParameters method)]
    (when-not (empty? type-parameters)
      (into [] type-parameters))))

(defn check-method [class-obj method-name argument-types]
  (let [methods (get-arity-methods class-obj method-name (count argument-types))]
    (case (count methods)
      0
      (throw (ex-info "method not found" {:class class-obj
                                          :method-name method-name
                                          :argument-types argument-types}))

      1
      (first methods)

      (throw (ex-info "ambiguous method signature" {:class class-obj
                                                    :method-name method-name
                                                    :argument-types argument-types
                                                    :methods methods})))))

(defn check-field [class-obj field-name]
  (try
    (.getField class-obj field-name)
    (catch java.lang.NoSuchFieldException e
      (throw (ex-info "field not found" {:ex e})))))

(defn try-get-field [class-obj field-name]
  (try
    (.getField class-obj field-name)
    (catch java.lang.NoSuchFieldException _ nil)))

(defn static? [member]
  (and member (-> member .getModifiers java.lang.reflect.Modifier/isStatic)))

(def annotated-type (comp :type meta))

(defn with-type
  ([exp t] (with-type exp t nil))
  ([exp t additionals] (vary-meta exp #(merge % {:type (if (class? t)
                                                         [(wrap-primitive-types t)]
                                                         (wrap-primitive-types t))} additionals))))

(def ^:dynamic *report-error (fn [_]))

(defn error
  ([msg] (error msg {}))
  ([msg args]
   (*report-error (assoc args :message msg))
   nil))

(defn try-get-method [class-obj method-name arg-types]
  (let [arity-methods (if class-obj
                        (get-arity-methods class-obj method-name (count arg-types))
                        [])]
    (when class-obj
      (case (count arity-methods)
        0 (error :method-not-found)

        1 (first arity-methods)

        (let [type-filtered-methods
              (filter (fn [method]
                        (and
                         (empty? (.getTypeParameters method))
                         (every? (fn [[param ta]]
                                   (= [(wrap-primitive-types (.getParameterizedType param))] (normalize ta)))
                                 (map vector (.getParameters method) arg-types)))) arity-methods)]
          (case (count type-filtered-methods)
            0 (error :no-method-overload-found {:arity arity-methods})
            1 (first type-filtered-methods)
            (error :ambiguous-method-signature {:method-candidates arity-methods})))))))

(defn get-variable [context variable-name]
  (get-in context [:st variable-name]))

(defn assoc-variable [context variable-name t]
  (assoc-in context [:st variable-name] t))

(defn get-type [context name]
  (get-in context [:types name]))

(defn assoc-type [context name t]
  (assoc-in context [:types name] t))

(defn get-level [context] (:level context 0))

(defn unify-message [t1 t2 message]
  (try
    (unify t1 t2)
    :unified
    (catch clojure.lang.ExceptionInfo e
      (if-let [unify-error (:unify/error (ex-data e))]
        (error message {:t1 t1 :t2 t2 :unify-error unify-error})
        (throw e)))))

(defn annotate-type [context t]
  (when (or (not (vector? t)) (empty? t))
    (throw (ex-info "annotate-type: unknown type format" {:type t})))
  (let [[qname & args] t
        n (if (= (count qname) 1) (first qname) qname)
        type
        (if
          (= qname ["Tuple"])
          (do
            (when (= (count args) 1)
              (error :tuple-cannot-have-arity-one {:args args}))
            :tuple)

          (if-let [{:keys [params]} (get-type context n)]
            (do
              (when-not (= (count params) (count args))
                (error :type-arity-mismatch {:params params :args args}))
              (keyword n))
            (let [type-name (clojure.string/join "." qname)
                  class-obj (try-get-class type-name)
                  _ (if class-obj
                      (let [type-params (.getTypeParameters class-obj)]
                        (when-not (= (count type-params) (count args))
                          (error :type-arity-mismatch {:params type-params :args args})))
                      (error :type-not-found {:name type-name}))]
              class-obj)))
        annotated-args (map (partial annotate-type context) args)]

    (with-type (into [qname] annotated-args) (into [type] (map annotated-type annotated-args)))))

;; https://www.logicbig.com/how-to/code-snippets/jcode-reflection-class-isassignablefrom.html
;; Object[] isAssignableFrom Integer[]: true

(defn assert-sub-type [error]
  (fn ass [[syntactic-type & syntactic-type-args] [inferred-type & inferred-type-args]]
    (when-not (.isAssignableFrom syntactic-type inferred-type)
      (error :upcast-invalid {:inferred-type inferred-type :upcast-type syntactic-type}))
    (doseq [[type-arg inferred-type-arg] (map vector syntactic-type-args inferred-type-args)]
      (ass type-arg inferred-type-arg))))

(defn annotate-pattern [context [kind & args]]
  (case kind
    :wildcard
    [{} (with-type [kind] (-> context get-level mk-type-var))]

    :pattern-identifier
    (let [[id] args
          parameter-type (-> context get-level mk-type-var)]
      (when (get-variable context id) (error :id-already-bound))
      [{id parameter-type}
       (with-type [kind id] parameter-type)])

    :type-annotation
    (let [[pat type] args
          [pv annotated-pattern] (annotate-pattern context pat)
          a-type (annotate-type context type)
          t (annotated-type a-type)]
      (unify-message t (annotated-type annotated-pattern) :type-does-not-match-annotation)
      [pv (with-type annotated-pattern t)])

    :constant
    (let [[c] args]
      [{} (with-type [kind c] (type c))])

    :tuple
    (let [[env pats]
          (reduce (fn [[env acc-pats] elem-pat]
                    (let [[elem-env a-elem-pat] (annotate-pattern context elem-pat)]
                      [(merge env elem-env) (conj acc-pats a-elem-pat)]))
                  [{} []]
                  args)]
      [env (with-type (into [:tuple] pats) (into [:tuple] (map annotated-type pats)))])))

(defn get-field [target-class field-name]
  (or (try-get-field target-class field-name) (error :field-not-found)))

(defn annotate-exp [context [kind & args :as exp]]
  (case kind
    :constant
    (with-type exp (-> args first type))

    ;; we have no syntax for this so maybe remove it?
    :class
    (let [[class-name] args
          class-obj (try-get-class class-name)]
      (when-not class-obj (error :class-not-found {:name class-name}))
      (with-type exp Class {:class class-obj}))

    :if
    (let [[an-cond an-true an-false] (map (partial annotate-exp context) args)]
      (unify-message [Boolean] (annotated-type an-cond) :if-cond-not-boolean)
      (unify-message (annotated-type an-true) (annotated-type an-false) :if-branches-differ)
      (with-type [kind an-cond an-true an-false] (annotated-type an-true)))

    :variable
    (let [[variable-name] args
          {:keys [tvars t]}
          (or (get-variable context variable-name)
              (do
                (error :variable-not-found {:variable-name variable-name})
                {:t Object}))
          spec-t (specialize (get-level context) (or tvars []) t)]
      (with-type exp spec-t))

    :upcast
    (let [[exp t] args
          annotated-exp (annotate-exp context exp)
          ann-type (annotate-type context t)
          syntactic-type (annotated-type ann-type)
          inferred-type (normalize (annotated-type annotated-exp))]
      (when (and syntactic-type inferred-type)
        ((assert-sub-type error) syntactic-type inferred-type))
      (with-type annotated-exp syntactic-type))

    :function
    (let [[ta tr] [(-> context get-level mk-type-var) (-> context get-level mk-type-var)]
          proc-case
          (fn [[parameter-pattern body]]
            (let [[pat-vars annotated-pattern] (annotate-pattern context parameter-pattern)
                  context1
                  (reduce (fn [c [v t]] (assoc-variable c v {:t t})) context pat-vars)
                  annotated-body (annotate-exp context1 body)]
              (unify-message ta (annotated-type annotated-pattern) :argument-pattern-types-differ)
              (unify-message tr (annotated-type annotated-body) :body-types-differ)
              [annotated-pattern annotated-body]))]
      (with-type (into [kind] (map proc-case args))
        [java.util.function.Function ta tr]))

    :field-access
    (let [[target field-name] args]
      (case (first target)
        :variable
        (let [qname (second target)
              class-obj (try-get-class (clojure.string/join "." qname))
              annotated-target (if class-obj
                                 (with-type qname class-obj)
                                 (annotate-exp context target))
              target-class (-> annotated-target annotated-type normalize first)
              field (get-field target-class field-name)
              t (when field (java-generic-type->type (.getGenericType field)))]
          (with-type [(if class-obj :get-static-field :get-instance-field) annotated-target field-name] t
            {:field field}))

        (let [annotated-target (annotate-exp context target)
              t (annotated-type annotated-target)
              nt (normalize t)
              field (get-field (first nt) field-name)
              ft (when field (java-generic-type->type (.getGenericType field)))]
          (with-type [:get-instance-field annotated-target field-name] ft {:field field}))))

    :let
    (let [[val-decls body] args
          [st2 a-val-decls]
          (reduce (fn [[ctx a-decls] [pat decl-exp]]
                    (let [ctx-level (update ctx :level (fnil inc 0))
                          [pat-vars annotated-pattern] (annotate-pattern ctx-level pat)
                          level (get-level ctx)
                          a-exp (annotate-exp ctx-level decl-exp)]
                      (unify-message (annotated-type annotated-pattern) (annotated-type a-exp) :val-decl)
                      [(reduce (fn [c [v t]]
                                 (assoc-variable
                                  c v
                                  (let [[tvars t] (generalize t level)]
                                    (merge {:t t} (when-not (empty? tvars) {:tvars tvars}))))) ctx pat-vars)
                       (conj a-decls [annotated-pattern a-exp])]))
                  [context []]
                  val-decls)
          a-body (annotate-exp st2 body)]
      (with-type [:let a-val-decls a-body] (annotated-type a-body)))

    :invoke-function
    (let [[func arg] args]
      (case (first func)
        :variable
        (let [[_ qname] func]
          (cond
            (get-variable context qname)
            (let [[annotated-func annotated-arg] (map (partial annotate-exp context) [func arg])
                  t-res (mk-type-var 0)]
              (unify-message [java.util.function.Function (annotated-type annotated-arg) t-res]
                             (annotated-type annotated-func) :argument-type-no-match)
              (with-type [kind annotated-func annotated-arg] t-res))

            :else
            (let [class-name (clojure.string/join "." qname)
                  class-obj (try-get-class class-name)
                  _ (when-not class-obj (error :class-not-found {:name class-name :qname qname}))
                  ctor-args (if (= (first arg) :tuple) (rest arg) [arg])

                  annotated-args (map (partial annotate-exp context) ctor-args)
                  arg-types (map (comp first annotated-type) annotated-args)
                  ctor (when class-obj
                         (let [ctor (try-get-constructor class-obj arg-types)]
                           (when-not ctor (error :constructor-not-found {:name class-name :arg-types arg-types}))
                           ctor))]
              (with-type (into [:construct class-name] annotated-args) class-obj {:ctor ctor}))))

        :field-access
        (let [[_ [kind & as]] func]
          (case kind
            :variable
            (let [qname (first as)
                  class-name (clojure.string/join "." qname)
                  method-name (last func)
                  method-args (if (= (first arg) :tuple) (rest arg) [arg])
                  class-obj (try-get-class class-name)
                  _ (when-not class-obj (error :class-not-found {:name class-name}))
                  annotated-args (map (partial annotate-exp context) method-args)
                  arg-types (map annotated-type (map wrap-primitive-types annotated-args))
                  method (try-get-method class-obj method-name arg-types)
                  _ (when (and method (not (static? method))) (error :not-static {:member method}))
                  [param-types return-type]
                  (if method
                    (specialize-method method)
                    [arg-types Object])]
              ;; todo handle when not a class!
              (doseq [[pt a-arg] (map vector param-types annotated-args)]
                (unify-message (wrap-primitive-types pt) (annotated-type a-arg) :argument-type-no-match))
              (with-type (into [:invoke-static-method class-name method-name] annotated-args) return-type
                {:method method}))

            (let [target (second func)
                  method-name (last func)
                  method-args (if (= (first arg) :tuple) (rest arg) [arg])
                  annotated-instance (annotate-exp context target)
                  annotated-args (map (partial annotate-exp context) method-args)
                  arg-types (map annotated-type (map wrap-primitive-types annotated-args))
                  method (try-get-method (first (annotated-type annotated-instance)) method-name arg-types)
                  _ (when (and method (static? method)) (error :static {:member method}))
                  [param-types return-type]
                  (if method
                    (specialize-method method)
                    [arg-types Object])]
              (doseq [[pt a-arg] (map vector param-types annotated-args)]
                (unify-message (wrap-primitive-types pt) (annotated-type a-arg) :argument-type-no-match))
              (with-type (into [:invoke-instance-method annotated-instance method-name] annotated-args) return-type
                {:method method}))))

        ;; assume it's a function
        (let [[annotated-func annotated-arg] (map (partial annotate-exp context) [func arg])
              t-res (mk-type-var 0)]
          (unify-message [java.util.function.Function (annotated-type annotated-arg) t-res]
                         (annotated-type annotated-func) :argument-type-no-match)
          (with-type [kind annotated-func annotated-arg] t-res))))

    :unary-minus
    (let [[e] args
          ae (annotate-exp context e)
          t  (annotated-type ae)]
      (when-not (-> t normalize first #{Long BigDecimal})
        (error :unary-minus-not-supported-on-type {:t t}))
      (with-type [kind ae] t))

    :binary-operator
    (let [[operator e1 e2] args
          [ae1 ae2] (map (partial annotate-exp context) [e1 e2])
          unified-operands (unify-message (annotated-type ae1) (annotated-type ae2) :operand-types-differ)
          operator-t (normalize (annotated-type ae1))
          result-type
          (cond
            (#{:+ :- :* :/} operator)
            (do
              (when (and unified-operands (not (#{Long BigDecimal} (first operator-t))))
                (error :no-overload-found {:operand-type operator-t}))
              operator-t)

            (#{:<= :>= :< :> :=} operator)
            (do
              (when (and unified-operands (not (#{Long BigDecimal #_ java.time.Instant} (first operator-t))))
                (error :comparison-does-not-apply {:operand-type operator-t}))
              [Boolean])

            (#{:&& :||} operator)
            (do
              (when (and unified-operands (not (= Boolean (first operator-t))))
                (error :logical-operator-on-non-boolean {:operand-type operator-t}))
              [Boolean])

            :else
            (throw (ex-info "unknown operator" {:operator operator}))
            )]
      (with-type [kind operator ae1 ae2] result-type))

    :tuple
    (let [a-exps (map (partial annotate-exp context) args)]
      (with-type (into [kind] a-exps) (into [:tuple] (map annotated-type a-exps))))

    (throw (ex-info "annotate-exp: unknown exp type" {:kind kind :exp exp}))))

(defn make-curried [param-types result]
  (reduce (fn [acc-t pt] [java.util.function.Function pt acc-t]) result param-types))

(defn annotate-top-level-decl [context [kind & args]]
  (case kind
    :val-decl
    (let [[pattern exp] args
          [pat-vars annotated-pattern] (annotate-pattern context pattern)
          annotated-exp (annotate-exp context exp)]
      (unify-message (annotated-type annotated-exp)
                     (annotated-type annotated-pattern) :val-definitions-differ)
      [(reduce
        (fn [c [v t]] (assoc-variable c v {:t t}))
        context
        pat-vars)
       [:val-decl annotated-pattern annotated-exp]])

    :type-decl
    (let [[type-name params kind & kind-args] args
          _ (when (= type-name "Tuple") (error :type-already-declared))
          _ (reduce (fn [acc type-param]
                      (when (acc type-param) (error :type-parameter-already-declared))
                      (conj acc type-param))
                    #{}
                    params)
          [annotated-kind-decl context1]
          (case kind
            :union
            (let [context-rec
                  (assoc-type context type-name {:params params})
                  annontated-ctors
                  (mapv (fn [[ctor-name & ctor-param-types]]
                          (into [ctor-name]
                                (map (partial annotate-type context-rec) ctor-param-types))) kind-args)
                  context1
                  (reduce
                   (fn [ctx [ctor-name & param-types]]
                     (when (get-variable ctx ctor-name) (error :constructor-redeclared))
                     (assoc-variable
                      ctx ctor-name
                      {:t (reduce (fn [acc-t pt] [java.util.function.Function (annotated-type pt) acc-t])
                                  (into [(keyword type-name)] param-types)
                                  (reverse param-types))}))
                   context
                   annontated-ctors)
                  _ (def *ctx context1)
                  ]
              [annontated-ctors context1])
            )
          ]
      [context1 [:type-decl type-name params kind annotated-kind-decl]])

    (throw (ex-info "annotate-top-level-decl: unknown type" {:kind kind}))))

(defn annotate-top-level-decls [context tlds]
  (reduce
   (fn [[ctx tlds] tld]
     (let [[ctx1 annotated-tld] (annotate-top-level-decl ctx tld)]
       [ctx1 (conj tlds annotated-tld)]))
   [context []]
   tlds))
