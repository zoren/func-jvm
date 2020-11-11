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

(defn check-constructor [class-obj parameter-types]
  (try
    (.getConstructor class-obj (into-array Class parameter-types))
    (catch java.lang.NoSuchMethodException e
      (throw (ex-info "constructor not found" {:ex e})))))

(def primitive-type->wrapper
  {Byte/TYPE Byte
   Short/TYPE Short
   Integer/TYPE Integer
   Long/TYPE Long
   Float/TYPE Float
   Double/TYPE Double
   Boolean/TYPE Boolean
   Character/TYPE Character})

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

(defn spec-method [method]
  (let [m (into {} (map (fn [tp] [tp (mk-type-var 0)]) (.getTypeParameters method)))
        type->
        (fn type-> [t]
          (cond
            (instance? java.lang.reflect.TypeVariable t)
            (m t)

            (instance? java.lang.reflect.ParameterizedType t)
            (into [(.getRawType ^java.lang.reflect.ParameterizedType t)]
                  (map type-> (.getActualTypeArguments ^java.lang.reflect.ParameterizedType t)))

            (class? t)
            [t]

            :else
            (throw (ex-info "not found" {:t t}))))]
    [(map #(type-> (.getParameterizedType %)) (.getParameters method))
     (type-> (.getGenericReturnType method))]))

(comment
  (spec-method
   (.getMethod java.lang.Long "getLong" (into-array Class [String Long])))

  (spec-method
   (.getMethod java.lang.Long "toHexString" (into-array Class [Long/TYPE])))

  (Long/toHexString 43)

  (bean (.getGenericReturnType (second (get-arity-methods java.util.List "of" 1))))

  (map (fn [m] (map #(.getName %) (.getTypeParameters m))) (get-arity-methods java.util.List "of" 1))
  (map (fn [m] (map #(.getParameterizedType %) (.getParameters m))) (get-arity-methods java.util.List "of" 1))
  
  (first
   (spec-method
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

(defn check-field [class-obj method-name]
  (try
    (.getField class-obj method-name)
    (catch java.lang.NoSuchFieldException e
      (throw (ex-info "field not found" {:ex e})))))

(defn static? [member]
  (-> member .getModifiers java.lang.reflect.Modifier/isStatic))

(def annotated-type (comp :type meta))

(defn with-type
  ([exp t] (with-type exp t nil))
  ([exp t additionals] (with-meta exp (merge {:type (if (class? t)
                                                      [(wrap-primitive-types t)]
                                                      (wrap-primitive-types t))} additionals))))

(defn annotate-exp [symbol-table [kind & args :as exp]]
  (case kind
    :constant
    (with-type exp (-> args first type))
    :class
    (with-type exp Class {:class (check-class (first args))})
    :get-static-field
    (let [[class-name field-name] args
          class-obj (check-class class-name)
          field (check-field class-obj field-name)]
      (when-not (static? field)
        (throw (ex-info "field not static" {:exp exp})))
      (with-type exp (.getType field) {:class class-obj :field field}))
    :get-instance-field
    (let [[instance-exp field-name] args
          annotated-instance (annotate-exp symbol-table instance-exp)
          field (check-field (first (annotated-type annotated-instance)) field-name)]
      (when (static? field)
        (throw (ex-info "field not instance" {:exp exp})))
      (with-type [kind annotated-instance field-name] (.getType field) {:field field}))
    :construct
    (let [[class-name & args] args
          c (check-class class-name)
          annotated-args (map (partial annotate-exp symbol-table) args)
          ctor (check-constructor c (map (comp first annotated-type) annotated-args))]
      (with-type (into [kind class-name] annotated-args) c {:ctor ctor}))
    :invoke-static-method
    (let [[class-name method-name & args] args
          annotated-args (map (partial annotate-exp symbol-table) args)
          method (check-method (check-class class-name) method-name
                               (map (comp first annotated-type) (map wrap-primitive-types annotated-args)))
          _ (when-not (static? method) (throw (ex-info "method not static" {:exp exp})))
          [param-types return-type] (spec-method method)]
      (doseq [[pt a-arg] (map vector param-types annotated-args)]
        (unify (wrap-primitive-types pt) (annotated-type a-arg)))
      (with-type (into [kind class-name method-name] annotated-args)
        return-type {:method method}))
    :invoke-instance-method
    (let [[instance-exp method-name & args] args
          annotated-instance (annotate-exp symbol-table instance-exp)
          annotated-args (map (partial annotate-exp symbol-table) args)
          method (check-method (first (annotated-type annotated-instance)) method-name
                               (map (comp first annotated-type) annotated-args))]
      (when (static? method)
        (throw (ex-info "method not instance" {:exp exp})))
      (with-type (into [kind annotated-instance method-name] annotated-args)
        (.getReturnType method) {:method method}))

    :if
    (let [[an-cond an-true an-false] (map (partial annotate-exp symbol-table) args)]
      #_(when-not (= Boolean (annotated-type an-cond))
          (throw (ex-info "condition was not boolean" {:exp exp})))
      (unify [Boolean] (annotated-type an-cond))
      (unify (annotated-type an-true) (annotated-type an-false))
      #_(when-not (= (annotated-type an-true) (annotated-type an-false))
          (throw (ex-info "if branches where different type"
                          {:exp exp :t-true (annotated-type an-true) :t-false (annotated-type an-false)})))

      (with-type [kind an-cond an-true an-false] (annotated-type an-true)))
    :var
    (do
      (when-not (contains? symbol-table (first args))
        (throw (ex-info "variable not found" {:exp exp})))
      (with-type exp (symbol-table (first args))))
    :upcast
    (let [[exp t] args
          annotated-exp (annotate-exp symbol-table exp)]
      (when-not (.isAssignableFrom t (first (annotated-type annotated-exp)))
        (throw (ex-info "upcast invalid: exp is not a sub-type of t" {:inferred-type (annotated-type annotated-exp)
                                                                      :upcast-type t})))
      (with-type annotated-exp t))

    (throw (ex-info "unknown exp type" {:exp exp}))))

(comment
  (annotate-exp {} [:invoke-static-method "java.util.List" "of" [:constant 42] [:constant "abc"]])
  (renumber
   (normalize (annotated-type (annotate-exp {} [:invoke-static-method "java.util.List" "of"]))))
  (renumber
   (normalize (annotated-type (annotate-exp {} [:invoke-static-method "java.util.List" "of" [:constant 42] [:constant 45]]))))

  )

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
    :var
    (-> args first env)

    (throw (ex-info "unknown exp type" {:exp exp}))))
