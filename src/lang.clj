(ns lang
  (:require
   [clojure.pprint :as pprint]
   [clojure.reflect :as reflect]
   )
  )

(def constant-types #{String Long})

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

(defn type-check [exp]
  (let [[kind & args] exp]
    (case kind
      :constant
      (type-constant (first args))
      :class
      (do
        (check-class (first args))
        Class)
      :construct
      (let [[class-name & args] args
            c (check-class class-name)]
        (check-constructor c (map type-check args))
        c)
      :invoke-static-method
      (let [[class-name method-name & args] args
            method (check-method (check-class class-name) method-name (map type-check args))]
        (when-not (static? method)
          (throw (ex-info "method not static" {:exp exp})))
        (.getReturnType method))
      :invoke-instance-method
      (let [[instance method-name & args] args
            method (check-method (type-check instance) method-name (map type-check args))]
        (when (static? method)
          (throw (ex-info "method not instance" {:exp exp})))
        (.getReturnType method))
      :get-static-field
      (let [[class-name field-name] args
            field (check-field (check-class class-name) field-name)]
        (when-not (static? field)
          (throw (ex-info "field not static" {:exp exp})))
        (.getType field))
      :get-instance-field
      (let [[instance field-name] args
            field (check-field (type-check instance) field-name)]
        (when (static? field)
          (throw (ex-info "field not instance" {:exp exp})))
        (.getType field))

      (throw (ex-info "unknown exp type" {:exp exp})))))

(defn eval-exp [exp]
  (let [[kind & args] exp]
    (case kind
      :constant
      (first args)
      :class
      (-> args first Class/forName)
      :construct
      (.newInstance
       (.getConstructor
        (-> args first Class/forName)
        (->> args rest (map type-check) (into-array Class)))
       (into-array Object (map eval-exp (rest args))))
      :invoke-static-method
      (let [[class-name method-name & args] args
            method (->> args (map type-check) (into-array Class) (.getMethod (Class/forName class-name) method-name))]
        (.invoke method nil (into-array Object (map eval-exp args))))
      :invoke-instance-method
      (let [[instance-exp method-name & args] args
            instance (eval-exp instance-exp)
            method (->> args (map type-check) (into-array Class) (.getMethod (class instance) method-name))]
        (.invoke method instance (into-array Object args)))
      :get-static-field
      (let [[class-name field-name] args
            c (Class/forName class-name)
            field (check-field c field-name)]
        (.get field c))
      :get-instance-field
      (let [[instance-exp field-name] args
            instance (eval-exp instance-exp)
            field (check-field (class instance) field-name)]
        (.get field instance))

      (throw (ex-info "unknown exp type" {:exp exp})))))

(comment
  (defn ppreflect
    "Pretty print a table describing all public members of the given object"
    [obj]
    (->> (reflect/reflect obj)
         :members
         (filter #(-> % :flags :public))
         (map #(assoc %
                      :kind (-> % type .getSimpleName)
                      :arity (count (:parameter-types %))))
                                        ;      (filter #(= (:kind %) "Constructor"))
         (sort-by (juxt :kind :name))
         (pprint/print-table [:kind :name :arity :parameter-types :return-type :flags])))
  )
