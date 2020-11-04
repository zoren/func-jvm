(ns lang-test
  (:require
   [clojure.test :refer [deftest is]]
   [lang :refer [annotate-exp eval-annotated-exp annotated-type]]
   )
  )

(deftest annotate-test
  (let [t (fn t
            ([e] (t {} e))
            ([st e] (:type (meta (annotate-exp st e)))))]
    (is (= Boolean (t [:constant false])))
    (is (= Boolean (t [:constant true])))
    (is (= String (t [:constant "abc"])))
    (is (= Long (t [:constant 123])))

    (is (thrown? clojure.lang.ExceptionInfo (t [:class "NoSuchClass"])))
    (is (= Class (t [:class "java.lang.Long"])))

    (is (thrown? clojure.lang.ExceptionInfo (t [:construct "java.lang.Long" [:constant 34]])))
    (is (= Object (t [:construct "java.lang.Object"])))
    (is (= Long (t [:construct "java.lang.Long" [:constant "23"]])))
    (is (= BigDecimal (t [:construct "java.math.BigDecimal" [:constant "23"]])))

    (is (thrown? clojure.lang.ExceptionInfo #"ambiguous"
                 (t [:invoke-static-method "java.lang.Long" "getLong" [:constant "java.specification.version"]
                     [:constant 34]])))
    (is (= String (t [:invoke-static-method "java.lang.Long" "toHexString" [:constant 42]])))

    (is (= String (t [:invoke-instance-method [:constant 12] "toString"])))
    (is (= Integer (t [:invoke-instance-method [:constant 12] "compareTo" [:constant 12]])))

    (is (= java.time.Month (t [:get-static-field "java.time.Month" "JULY"])))

    (is (= Long (t [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"])))

    (is (thrown? clojure.lang.ExceptionInfo (t [:if [:constant "not bool"] [:constant 3] [:constant 5]])))
    (is (thrown? clojure.lang.ExceptionInfo (t [:if [:constant true] [:constant 3] [:constant "not same type as 3"]])))
    (is (= Long (t [:if [:constant true] [:constant 3] [:constant 5]])))
    (is (= String (t {:x String} [:var :x])))))

(deftest eval-annotated-exp-test
  (let [eval-exp (fn eval-exp
                   ([exp] (eval-exp {} exp))
                   ([env exp] (eval-annotated-exp env (annotate-exp {} exp))))]
    (is (= "abc" (eval-exp [:constant "abc"])))
    (is (= 123 (eval-exp [:constant 123])))

    (is (= Long (eval-exp [:class "java.lang.Long"])))

    (is (= Object (type (eval-exp [:construct "java.lang.Object"]))))
    (is (= "abc" (eval-exp [:construct "java.lang.String" [:constant "abc"]])))
    (is (= 123 (eval-exp [:construct "java.lang.Long" [:constant "123"]])))

    (is (= 14 (eval-exp [:invoke-static-method "java.lang.Long" "valueOf" [:constant "14"]])))

    (is (= "123" (eval-exp [:invoke-instance-method [:constant 123] "toString"])))
    (is (= 6 (eval-exp [:invoke-instance-method [:construct "experimentation.java.PublicInstanceField"] "plus"])))

    (is (= java.time.Month/JULY (eval-exp [:get-static-field "java.time.Month" "JULY"])))

    (is (= 5
           (eval-exp [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"])))

    (is (= 3 (eval-exp [:if [:constant true] [:constant 3] [:constant 5]])))
    (is (= 5 (eval-exp [:if [:constant false] [:constant 3] [:constant 5]])))

    (is (= "abc" (eval-exp {:x "abc"} [:var :x])))))

(deftest preservation-test
  (let [pres (fn pres
               ([exp t]
                (pres {} exp t))
               ([st-env exp t]
                (let [annotated-exp (annotate-exp
                                     (into {} (map (fn [[variable [t _value]]] [variable t]) st-env)) exp)]
                  (is (= t (annotated-type annotated-exp)))
                  (is (= t (class (eval-annotated-exp
                                   (into {} (map (fn [[variable [_t value]]] [variable value]) st-env)) annotated-exp)))))))]
    (pres [:constant "abc"] String)
    (pres [:constant 123] Long)

    (pres [:class "java.lang.Long"] Class)

    (pres [:construct "java.lang.Object"] Object)
    (pres [:construct "java.lang.Long" [:constant "123"]] Long)
    (pres [:construct "java.math.BigDecimal" [:constant "123"]] BigDecimal)

    (pres [:invoke-static-method "java.lang.Long" "valueOf" [:constant "14"]] Long)

    (pres [:invoke-instance-method [:constant 12] "toString"] String)

    (pres [:get-static-field "java.time.Month" "JULY"] java.time.Month)

    (pres [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"] Long)

    (pres {:x [Long 42]} [:var :x] Long)))
