(ns lang)

(def constant-types #{String Long Boolean})

(defn type-constant [exp]
  (constant-types (type exp)))

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

(defn check-method [class-obj method-name parameter-types]
  (try
    (.getMethod class-obj method-name (into-array Class parameter-types))
    (catch java.lang.NoSuchMethodException e
      (throw (ex-info "method not found" {:ex e})))))

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
  ([exp t additionals] (with-meta exp (merge {:type t} additionals))))

(defn annotate [[kind & args :as exp]]
  (case kind
    :constant
    (with-type exp (-> args first type-constant))
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
          annotated-instance (annotate instance-exp)
          field (check-field (annotated-type annotated-instance) field-name)]
      (when (static? field)
        (throw (ex-info "field not instance" {:exp exp})))
      (with-type [kind annotated-instance field-name] (.getType field) {:field field}))
    :construct
    (let [[class-name & args] args
          c (check-class class-name)
          annotated-args (map annotate args)
          ctor (check-constructor c (map annotated-type annotated-args))]
      (with-type (into [kind class-name] annotated-args) c {:ctor ctor}))
    :invoke-static-method
    (let [[class-name method-name & args] args
          annotated-args (map annotate args)
          method (check-method (check-class class-name) method-name (map annotated-type annotated-args))]
      (when-not (static? method)
        (throw (ex-info "method not static" {:exp exp})))
      (with-type (into [:invoke-static-method class-name method-name] annotated-args)
        (.getReturnType method) {:method method}))
    :invoke-instance-method
    (let [[instance-exp method-name & args] args
          annotated-instance (annotate instance-exp)
          annotated-args (map annotate args)
          method (check-method (annotated-type annotated-instance) method-name (map annotated-type annotated-args))]
      (when (static? method)
        (throw (ex-info "method not instance" {:exp exp})))
      (with-type (into [:invoke-instance-method annotated-instance method-name] annotated-args)
        (.getReturnType method) {:method method}))

    :if
    (let [[an-cond an-true an-false] (map annotate args)]
      (when-not (= Boolean (annotated-type an-cond))
        (throw (ex-info "condition was not boolean" {:exp exp})))
      (when-not (= (annotated-type an-true) (annotated-type an-false))
        (throw (ex-info "if branches where different type"
                        {:exp exp :t-true (annotated-type an-true) :t-false (annotated-type an-false)})))
      (with-type [:if an-cond an-true an-false] (annotated-type an-true)))

    (throw (ex-info "unknown exp type" {:exp exp}))))

(defn eval-annotated-exp [[kind & args :as exp]]
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
          instance (eval-annotated-exp instance-exp)
          field (-> exp meta :field)]
      (.get field instance))
    :construct
    (let [ctor (-> exp meta :ctor)]
      (.newInstance
       ctor
       (into-array Object (map eval-annotated-exp (rest args)))))
    :invoke-static-method
    (let [[_class-name _method-name & args] args
          method (-> exp meta :method)]
      (.invoke method nil (into-array Object (map eval-annotated-exp args))))
    :invoke-instance-method
    (let [[instance-exp _method-name & args] args
          instance (eval-annotated-exp instance-exp)
          method (-> exp meta :method)]
      (.invoke method instance (into-array Object args)))

    :if
    (let [[cond t f] args]
      (if (eval-annotated-exp cond) (eval-annotated-exp t) (eval-annotated-exp f)))

    (throw (ex-info "unknown exp type" {:exp exp}))))

