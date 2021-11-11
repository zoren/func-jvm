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
                                        ;       (filter #(= (:kind %) "Constructor"))
       ;;       (filter (fn [r] (and (= (:kind r) "Method") (not ((:flags r) :static) ))))
       (sort-by (juxt :kind :name))
       (pprint/print-table [:kind :name :arity :parameter-types :return-type :flags])))

(ppreflect 12)
(ppreflect java.lang.StringBuilder)
#_(def wrapper->primitive-type
    (into {} (map (fn [[k v]] [v k]) primitive-type->wrapper)))

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


(first (get-arity-methods java.lang.Long "getLong" 2))

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

(get-type-parameters (first (get-arity-methods java.lang.Long "getLong" 2)))
(bean (first (get-type-parameters (first (get-arity-methods java.util.List "of" 2)))))

(comment
  (check-method java.util.List "of" [])
  (check-method java.util.List "of" [Long Long])
  (check-method java.util.List "of" (repeat 20 Long))
  )



(defrecord R [x y z]
  )

(def n 'REC)
n
(eval `(defrecord ~n []
         ))

(REC.)

(def n 'REC)
(eval `(defrecord ~n [~'x ~'y]))
(REC. 4 5)




#_
(= (atld "val x = y"))
#_
(is (= "" "
type Bool
  | False
  | True

type RuntimeError
  | InexhaustiveMatchError
  | DivisionByZeroError
  | EvaluationError String

type Maybe a
  | None
  | Some a

type List a
  | Nil
  | Cons a (List a)
"
       ))
#_
(is (= "fail because no equality function can be generated"
       (pt "type F | F (Float -> Float)

val gs = \\(g:List F) -> \\(gg:List F) -> g = gg"))

    )
(annotate-top-level-decl {} [:type-decl "B" nil :union ["F"] ["T"]])

(annotate-top-level-decl {} (first (antlr/parse-top-level "type B | F java::lang::Lon | T")))
(reduce (fn [t param-t] [:func param-t t]) :r (reverse [:a :b :c]))
