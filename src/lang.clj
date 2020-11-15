(ns lang
  (:require
   [unify :refer [mk-type-var unify normalize renumber]]
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
  ([exp t additionals] (with-meta exp (merge {:type (if (class? t)
                                                      [(wrap-primitive-types t)]
                                                      (wrap-primitive-types t))} additionals))))

(defn try-get-method [error class-obj method-name arg-types]
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

(defn annotate-exp [symbol-table]
  (let [errors (atom [])
        error (fn error
                ([message] (error message {}))
                ([message args] (swap! errors conj (assoc args :message message))
                 nil))
        unify-message
        (fn [t1 t2 message]
          (try
            (unify t1 t2)
            nil
            (catch clojure.lang.ExceptionInfo _e
              (error message {:t1 t1 :t2 t2}))))
        try-get-method (partial try-get-method error)
        a-exp
        (fn a-exp [[kind & args :as exp]]
          (case kind
            :constant
            (with-type exp (-> args first type))

            :class
            (let [[class-name] args
                  class-obj (try-get-class class-name)]
              (when-not class-obj (error :class-not-found {:name class-name}))
              (with-type exp Class {:class class-obj}))

            :construct
            (let [[class-name & args] args
                  class-obj (try-get-class class-name)
                  _ (when-not class-obj (error :class-not-found {:name class-name}))
                  annotated-args (map a-exp args)
                  arg-types (map (comp first annotated-type) annotated-args)
                  ctor (when class-obj
                         (let [ctor (try-get-constructor class-obj arg-types)]
                           (when-not ctor (error :constructor-not-found {:name class-name :arg-types arg-types}))
                           ctor))]
              (with-type (into [kind class-name] annotated-args) class-obj {:ctor ctor}))

            :get-static-field
            (let [[class-name field-name] args
                  class-obj (try-get-class class-name)
                  _ (when-not class-obj (error :class-not-found {:name class-name}))
                  field (when class-obj
                          (let [field (try-get-field class-obj field-name)]
                            (when-not field (error :field-not-found))
                            field))
                  t (if field (java-generic-type->type (.getGenericType field)) [Object])]
              (when (and field (not (static? field))) (error :not-static {:member field}))
              (with-type exp t {:class class-obj :field field}))

            :get-instance-field
            (let [[instance-exp field-name] args
                  annotated-instance (a-exp instance-exp)
                  field (try-get-field (first (annotated-type annotated-instance)) field-name)
                  t (if field
                      (do
                        (when (static? field) (error :static))
                        (java-generic-type->type (.getGenericType field)))
                      (do
                        (error :field-not-found)
                        Object))]
              (with-type [kind annotated-instance field-name] t {:field field}))

            :invoke-static-method
            (let [[class-name method-name & args] args
                  class-obj (try-get-class class-name)
                  _ (when-not class-obj (error :class-not-found {:name class-name}))
                  annotated-args (map a-exp args)
                  arg-types (map annotated-type (map wrap-primitive-types annotated-args))
                  method (try-get-method class-obj method-name arg-types)
                  _ (when (and method (not (static? method))) (error :not-static {:member method}))
                  [param-types return-type]
                  (if method
                    (specialize-method method)
                    [arg-types Object])]
              (doseq [[pt a-arg] (map vector param-types annotated-args)]
                (unify-message (wrap-primitive-types pt) (annotated-type a-arg) :argument-type-no-match))
              (with-type (into [kind class-name method-name] annotated-args) return-type {:method method}))

            :invoke-instance-method
            (let [[instance-exp method-name & args] args
                  annotated-instance (a-exp instance-exp)
                  annotated-args (map a-exp args)
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
            (let [[an-cond an-true an-false] (map a-exp args)]
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
                  annotated-exp (a-exp exp)
                  inferred-type (annotated-type annotated-exp)]
              ;; TODO, check arity of type
              (when-not (unify/type? t) (error :not-a-type {:type t}))
              ;; todo what about type params/args?
              (when-not (.isAssignableFrom (first t) (first inferred-type))
                (error :upcast-invalid {:inferred-type inferred-type :upcast-type t}))
              ;; todo this won't work generally, we need a general subtyping concept as in java
              (doseq [[type-arg inferred-type-arg] (map vector (rest t) (rest inferred-type))]
                (when-not (.isAssignableFrom (first type-arg) (first (normalize inferred-type-arg)))
                  (error :upcast-invalid-type-arg {:type-arg type-arg
                                                   :inferred-type-arg inferred-type-arg})))
              (with-type annotated-exp t))

            (throw (ex-info "unknown exp type" {:exp exp}))))
        ]
    (fn [e] {:annotated-exp (a-exp e) :errors @errors})
    ))

;; https://www.logicbig.com/how-to/code-snippets/jcode-reflection-class-isassignablefrom.html
;; Object[] isAssignableFrom Integer[]: true

;; (comment
;;   (annotate-exp {} [:invoke-static-method "java.util.List" "of" [:constant 42] [:constant "abc"]])
;;   (renumber
;;    (normalize (annotated-type (annotate-exp {} [:invoke-static-method "java.util.List" "of"]))))
;;   (renumber
;;    (normalize (annotated-type (annotate-exp {} [:invoke-static-method "java.util.List" "of" [:constant 42] [:constant 45]]))))

;;   )

(defn fn->function [f]
  (reify java.util.function.Function
    (apply [this a]
      (f a))))

;; :fn
;; (let [[fn-params body] args
;;       a-body (annotate-exp (into symbol-table fn-params) body)]
;;   (with-type (annotate-exp (into symbol-table fn-params) body)
;;     ()
;;     )
;;   )

;; (class java.util.function.Function)
;; java.util.function.Function


;; (-> [2]
;;     .stream
;;     (.map (fn->function #(* % 5)))
;;     (.collect (java.util.stream.Collectors/toList))
;;     )


(defn eval-annotated-exp [env [kind & args :as exp]]
  (case kind
    :constant
    (first args)
    :class
    (-> exp meta :class)
    :get-static-field
    (let [class-obj (-> exp meta :class)
          field (-> exp meta :field)]
      (.get field class-obj))
    :get-instance-field
    (let [[instance-exp _field-name] args
          instance (eval-annotated-exp env instance-exp)
          field (-> exp meta :field)]
      (.get field instance))
    :construct
    (let [ctor (-> exp meta :ctor)]
      (.newInstance
       ctor
       (into-array Object (map (partial eval-annotated-exp env) (rest args)))))
    :invoke-static-method
    (let [[_class-name _method-name & args] args
          method (-> exp meta :method)]
      (.invoke method nil (into-array Object (map (partial eval-annotated-exp env) args))))
    :invoke-instance-method
    (let [[instance-exp _method-name & args] args
          instance (eval-annotated-exp env instance-exp)
          method (-> exp meta :method)]
      (.invoke method instance (into-array Object (map (partial eval-annotated-exp env) args))))

    :if
    (let [[cond t f] args]
      (if (eval-annotated-exp env cond) (eval-annotated-exp env t) (eval-annotated-exp env f)))
    :variable
    (-> args first env)

    (throw (ex-info "unknown exp type" {:exp exp}))))
