(ns lang-test
  (:require
   [clojure.test :refer [deftest is]]
   [lang :refer [annotate-exp eval-annotated-exp]]
   )
  )

(deftest annotate-test
  (let [t (fn [e] (-> e annotate-exp meta :type))]
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

    (is (= Long (t [:invoke-static-method "java.lang.Long" "getLong" [:constant "java.specification.version"]
                    [:constant 34]])))

    (is (= String (t [:invoke-instance-method [:constant 12] "toString"])))

    (is (= java.time.Month (t [:get-static-field "java.time.Month" "JULY"])))

    (is (= Long (t [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"])))

    (is (thrown? clojure.lang.ExceptionInfo (t [:if [:constant "not bool"] [:constant 3] [:constant 5]])))
    (is (thrown? clojure.lang.ExceptionInfo (t [:if [:constant true] [:constant 3] [:constant "not same type as 3"]])))
    (is (= Long (t [:if [:constant true] [:constant 3] [:constant 5]])))))

(deftest eval-annotated-exp-test
  (let [eval-exp (fn [exp] (eval-annotated-exp (annotate-exp exp)))]
    (is (= "abc" (eval-exp [:constant "abc"])))
    (is (= 123 (eval-exp [:constant 123])))

    (is (= Long (eval-exp [:class "java.lang.Long"])))

    (is (= Object (type (eval-exp [:construct "java.lang.Object"]))))
    (is (= "abc" (eval-exp [:construct "java.lang.String" [:constant "abc"]])))
    (is (= 123 (eval-exp [:construct "java.lang.Long" [:constant "123"]])))

    (is (= 14
           (eval-exp [:invoke-static-method "java.lang.Long" "getLong" [:constant "java.specification.version"]
                      [:constant 34]])))

    (is (= "123" (eval-exp [:invoke-instance-method [:constant 123] "toString"])))
    (is (= 6 (eval-exp [:invoke-instance-method [:construct "experimentation.java.PublicInstanceField"] "plus"])))

    (is (= java.time.Month/JULY (eval-exp [:get-static-field "java.time.Month" "JULY"])))

    (is (= 5
           (eval-exp [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"])))

    (is (= 3 (eval-exp [:if [:constant true] [:constant 3] [:constant 5]])))
    (is (= 5 (eval-exp [:if [:constant false] [:constant 3] [:constant 5]])))))

(deftest preservation-test
  (let [pres (fn [exp t]
               (let [annotated-exp (annotate-exp exp)]
                 (is (= t (-> annotated-exp meta :type)))
                 (is (= t (class (eval-annotated-exp annotated-exp))))))]
    (pres [:constant "abc"] String)
    (pres [:constant 123] Long)

    (pres [:class "java.lang.Long"] Class)

    (pres [:construct "java.lang.Object"] Object)
    (pres [:construct "java.lang.Long" [:constant "123"]] Long)
    (pres [:construct "java.math.BigDecimal" [:constant "123"]] BigDecimal)

    (pres [:invoke-static-method "java.lang.Long" "getLong" [:constant "java.specification.version"] [:constant 34]] Long)

    (pres [:invoke-instance-method [:constant 12] "toString"] String)

    (pres [:get-static-field "java.time.Month" "JULY"] java.time.Month)

    (pres [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"] Long)))
