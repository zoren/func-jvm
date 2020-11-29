(ns annotate
  (:require
   [clojure.string]
   [unify :refer [mk-type-var unify normalize]]
   )
  )

(defn check-class [class-name]
  (try
    (Class/forName class-name)
    (catch java.lang.ClassNotFoundException e
      (throw (ex-info "class not found" {:class-name class-name :ex e})))))

(defn try-get-class [class-name]
  (try
    (Class/forName class-name)
    (catch java.lang.ClassNotFoundException _ nil)))

(defn check-constructor [class-obj parameter-types]
  (try
    (.getConstructor class-obj (into-array Class parameter-types))
    (catch java.lang.NoSuchMethodException e
      (throw (ex-info "constructor not found" {:ex e})))))

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
  (if (and (vector? t) (= 1 (count t)))
    [(primitive-type->wrapper (first t) (first t))]
    (primitive-type->wrapper t t)))

(bean
 (first
  (.getParameters
   (first (filter (fn [method]
                    (and (= (.getName method) "of")))
                  (.getMethods java.util.List))))))

(-> java.util.List
    (.getMethod "of" (into-array Class [Object Object Object]))
    .getTypeParameters
    first)

(empty?
 (->>
  (-> java.lang.Long
      .getMethods)
  (filter (fn [method]
            (and (= (.getName method) "getLong"))
            ))
  last
  .getTypeParameters
  )
 )

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

(first (get-arity-methods java.lang.Long "getLong" 2))

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

(comment
  (specialize-method
   (.getMethod java.lang.Long "getLong" (into-array Class [String Long])))
  (empty? (.getTypeParameters (.getMethod java.lang.Long "getLong" (into-array Class [String Long]))))
  (specialize-method
   (.getMethod java.lang.Long "toHexString" (into-array Class [Long/TYPE])))

  (Long/toHexString 43)

  (bean (.getGenericReturnType (second (get-arity-methods java.util.List "of" 1))))

  (map (fn [m] (map #(.getName %) (.getTypeParameters m))) (get-arity-methods java.util.List "of" 1))
  (map (fn [m] (map #(.getParameterizedType %) (.getParameters m))) (get-arity-methods java.util.List "of" 1))

  (first
   (specialize-method
    (first (get-arity-methods java.util.List "of" 1))
    )
   )
  )


(defn get-type-parameters [method]
  (let [type-parameters (.getTypeParameters method)]
    (when-not (empty? type-parameters)
      (into [] type-parameters))))

(get-type-parameters (first (get-arity-methods java.lang.Long "getLong" 2)))
(bean (first (get-type-parameters (first (get-arity-methods java.util.List "of" 2)))))

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

(comment
  (check-method java.util.List "of" [])
  (check-method java.util.List "of" [Long Long])
  (check-method java.util.List "of" (repeat 20 Long))
  )

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

(def errors (atom []))

(defn error
  ([msg] (error msg {}))
  ([msg args] (do (swap! errors conj (assoc args :message msg)) nil)))

(comment
  (reset! errors [])
  )

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
            0 (error :method-not-found {:arity arity-methods})
            1 (first type-filtered-methods)
            (error :ambiguous-method-signature {:method-candidates arity-methods})))))))

(defn unify-message [t1 t2 message]
  (try
    (unify t1 t2)
    nil
    (catch clojure.lang.ExceptionInfo _e
      (error message {:t1 t1 :t2 t2}))))

(defn annotate-type [t]
  (when (or (not (vector? t)) (empty? t))
    (throw (ex-info "annotate-type: unknown type format" {:type t})))
  (let [[qname & args] t
        class-name (clojure.string/join "." qname)
        class-obj (try-get-class class-name)
        _ (if class-obj
            (when-not (= (count (.getTypeParameters class-obj)) (count args))
              (error :type-arity-mismatch {:params (.getTypeParameters class-obj) :args args}))
            (error :class-not-found {:name class-name}))
        annotated-args (map annotate-type args)]

    (with-type (into [class-name] annotated-args) (into [class-obj] (map annotated-type annotated-args)))))

;; https://www.logicbig.com/how-to/code-snippets/jcode-reflection-class-isassignablefrom.html
;; Object[] isAssignableFrom Integer[]: true

(defn assert-sub-type [error]
  (fn ass [[syntactic-type & syntactic-type-args] [inferred-type & inferred-type-args]]
    (when-not (.isAssignableFrom syntactic-type inferred-type)
      (error :upcast-invalid {:inferred-type inferred-type :upcast-type syntactic-type}))
    (doseq [[type-arg inferred-type-arg] (map vector syntactic-type-args inferred-type-args)]
      (ass type-arg inferred-type-arg))))

(defn annotate-pattern [symbol-table [kind & args]]
  (case kind
    :pattern-identifier
    (let [[id] args
          parameter-type (mk-type-var 0)]
      (when (symbol-table id) (error :id-already-bound))
      [(assoc symbol-table id parameter-type)
       (with-type [kind id] parameter-type)])

    :type-annotation
    (let [[pat type] args
          [st2 annotated-pattern] (annotate-pattern symbol-table pat)
          a-type (annotate-type type)
          t (annotated-type a-type)]
      (unify-message t (annotated-type annotated-pattern) :type-does-not-match-annotation)
      [st2
       (with-type [kind annotated-pattern annotated-type] t)])
    ))

(defn get-field [target-class field-name]
  (or (try-get-field target-class field-name) (error :field-not-found)))

(defn annotate-exp [symbol-table [kind & args :as exp]]
  (case kind
    :constant
    (with-type exp (-> args first type))

    :class
    (let [[class-name] args
          class-obj (try-get-class class-name)]
      (when-not class-obj (error :class-not-found {:name class-name}))
      (with-type exp Class {:class class-obj}))

    :invoke-instance-method
    (let [[instance-exp method-name & args] args
          annotated-instance (annotate-exp symbol-table instance-exp)
          annotated-args (map (partial annotate-exp symbol-table) args)
          arg-types (map annotated-type (map wrap-primitive-types annotated-args))
          method (try-get-method (first (annotated-type annotated-instance)) method-name arg-types)
          _ (when (and method (static? method)) (error :static {:member method}))
          [param-types return-type]
          (if method
            (specialize-method method)
            [arg-types Object])]
      (doseq [[pt a-arg] (map vector param-types annotated-args)]
        (unify-message (wrap-primitive-types pt) (annotated-type a-arg) :argument-type-no-match))
      (with-type (into [kind annotated-instance method-name] annotated-args) return-type {:method method}))

    :if
    (let [[an-cond an-true an-false] (map (partial annotate-exp symbol-table) args)]
      (unify-message [Boolean] (annotated-type an-cond) :if-cond-not-boolean)
      (unify-message (annotated-type an-true) (annotated-type an-false) :if-branches-differ)
      (with-type [kind an-cond an-true an-false] (annotated-type an-true)))

    :variable
    (let [[variable-name] args]
      (when-not (contains? symbol-table variable-name)
        (error :variable-not-found {:variable-name variable-name}))
      (with-type exp (symbol-table variable-name)))

    :upcast
    (let [[exp t] args
          annotated-exp (annotate-exp symbol-table exp)
          ann-type (annotate-type t)
          syntactic-type (annotated-type ann-type)
          inferred-type (normalize (annotated-type annotated-exp))]
      ((assert-sub-type error) syntactic-type inferred-type)
      (with-type annotated-exp syntactic-type))

    :function
    (let [[parameter-pattern body] args
          [symbol-table1 annotated-pattern] (annotate-pattern symbol-table parameter-pattern)
          annotated-body (annotate-exp symbol-table1 body)]
      (with-type [kind annotated-pattern annotated-body]
        [java.util.function.Function (annotated-type annotated-pattern) (annotated-type annotated-body)]))

    :field-access
    (let [[target field-name] args]
      (case (first target)
        :variable
        (let [qname (second target)
              class-obj (try-get-class (clojure.string/join "." qname))
              annotated-target (if class-obj
                                 (with-type qname class-obj)
                                 (annotate-exp symbol-table target))
              target-class (-> annotated-target annotated-type normalize first)
              field (get-field target-class field-name)
              t (java-generic-type->type (.getGenericType field))]
          (with-type [(if class-obj :get-static-field :get-instance-field) annotated-target field-name] t {:field field}))

        (let [annotated-target (annotate-exp symbol-table target)
              t (annotated-type annotated-target)
              nt (normalize t)
              field (get-field (first nt) field-name)
              ft (java-generic-type->type (.getGenericType field))]
          (with-type [:get-instance-field annotated-target field-name] ft {:field field}))))

    :invoke-function
    (let [[func arg] args]
      (case (first func)
        :variable
        (let [[_ qname] func
              class-name (clojure.string/join "." qname)
              class-obj (try-get-class class-name)
              _ (when-not class-obj (error :class-not-found {:name class-name :qname qname}))
              ctor-args (if (= (first arg) :tuple) (rest arg) [arg])

              annotated-args (map (partial annotate-exp symbol-table) ctor-args)
              arg-types (map (comp first annotated-type) annotated-args)
              ctor (when class-obj
                     (let [ctor (try-get-constructor class-obj arg-types)]
                       (when-not ctor (error :constructor-not-found {:name class-name :arg-types arg-types}))
                       ctor))
              ]
          (with-type (into [:construct class-name] annotated-args) class-obj {:ctor ctor})
          )

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
                  annotated-args (map (partial annotate-exp symbol-table) method-args)
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
              (with-type (into [:invoke-static-method class-name method-name] annotated-args) return-type {:method method}))

            (let [method-args (if (= (first arg) :tuple) (rest arg) [arg])]
              (annotate-exp symbol-table (into [:invoke-instance-method (second func) (last func)] method-args)))))

        ;; assume it's a function
        (let [[annotated-func annotated-arg] (map (partial annotate-exp symbol-table) [func arg])
              t-res (mk-type-var 0)]
          (unify-message [java.util.function.Function (annotated-type annotated-arg) t-res]
                         (annotated-type annotated-func) :argument-type-no-match)
          (with-type [kind annotated-func annotated-arg] t-res))))

    (throw (ex-info "annotate-exp: unknown exp type" {:kind kind :exp exp}))))

(defn annotate-top-level-decl [symbol-table [kind & args]]
  (case kind
    :val-decl
    (let [[pattern exp] args
          [symbol-table1 annotated-pattern] (annotate-pattern symbol-table pattern)
          annotated-exp (annotate-exp symbol-table1 exp)]
      (unify-message (annotated-type annotated-exp) (annotated-type annotated-pattern) :val-definitions-differ)
      [symbol-table1 [:val-decl annotated-pattern annotated-exp]])))

(defn annotate-top-level-decls [symbol-table tlds]
  (reduce (fn [[st tlds] tld]
            (let [[st1 annotated-tld] (annotate-top-level-decls st tld)]
              [st1 (conj tlds annotated-tld)])) [symbol-table []] tlds))
