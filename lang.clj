(ns user
  (:require
   [clojure.pprint :as pprint]
   [clojure.reflect :as reflect]
   )
  )

(defn ppreflect
  "Pretty print a table describing all public members of the given object"
  [obj]
  (->> (reflect/reflect obj)
       :members
       (filter #(-> % :flags :public))
       (map #(assoc %
                    :kind (-> % type .getSimpleName)
                    :arity (count (:parameter-types %))))
       (filter #(= (:kind %) "Constructor"))
       (sort-by (juxt :kind :name))
       (pprint/print-table [:kind :name :arity :parameter-types :return-type :flags])))

(ppreflect Integer)
(ppreflect Long)

(Long. 3)

(def constant-types #{String Long})

(defn type-constant [exp]
  (constant-types (type exp)))

(defn check-class [class-name]
  (try
    (Class/forName class-name)
    (catch java.lang.ClassNotFoundException e
      (throw (ex-info "class not found" {:class-name class-name :ex e})))))

(defn type-check [exp]
  (if-let [t (type-constant exp)]
    t
    (case (first exp)
      :class
      (do
        (check-class (second exp))
        Class)
      :construct
      (let [c (check-class (second exp))
            args (map type-check (drop 2 exp))]
        (try
          (.getConstructor c (into-array Class args))
          c
          (catch java.lang.NoSuchMethodException e
            (throw (ex-info "constructor not found" {:exp exp :args args :ex e})))))

      (throw (ex-info "unknown exp type" {:exp exp})))))

(defn eval-exp [exp]
  (if (type-constant exp)
    exp
    (case (first exp)
      :class
      (Class/forName (second exp))
      :construct
      (.newInstance
       (.getConstructor
        (Class/forName (second exp))
        (into-array Class (map type (drop 2 exp))))
       (into-array Object (drop 2 exp)))

      (throw (ex-info "unknown exp type" {:exp exp})))))

(defn comp [exp])

(comment
  (ppreflect java.lang.AtomicInteger)
  (type-check [:class "d"])
  (reflect/reflect Integer)
  (ppreflect Integer)

  (type-check "abc")
  (type-check 123)
  (type-check [:class "java.lang.Integer"])
  (type-check [:class "NoSuchClass"])
  (type-check [:construct "java.lang.Object"])
  (type-check [:construct "java.lang.Long" "23"])
  (type-check [:construct "java.lang.Long" 34])
  (eval-exp [:construct "java.lang.Long" "23"])
  (type-check [:construct "java.math.BigDecimal" "23"])
  (eval-exp [:construct "java.lang.Long" 34])
                                        ;  (Object.)
  (->
   [:construct "java.lang.Integer" ["123"]]
   )

  (-> "java.lang.Integer"
      Class/forName
      (.getConstructor
       (into-array Class [String String])
       )
      )

  (-> "java.lang.Long"
      Class/forName
      (.getConstructor
       (into-array Class [(Class/forName "java.lang.Long")])
       )
      (.newInstance
       (into-array Object [123])
       )
      )
  )
