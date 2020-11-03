(ns lang-test
  (:require
   [clojure.test :refer [deftest is]]
   [lang :refer [type-check eval-exp]]
   )
  )

(deftest type-check-test
  (is (= String (type-check [:constant "abc"])))
  (is (= Long (type-check [:constant 123])))

  (is (thrown? clojure.lang.ExceptionInfo (type-check [:class "NoSuchClass"])))
  (is (= Class (type-check [:class "java.lang.Long"])))

  (is (thrown? clojure.lang.ExceptionInfo (type-check [:construct "java.lang.Long" [:constant 34]])))
  (is (= Object (type-check [:construct "java.lang.Object"])))
  (is (= Long (type-check [:construct "java.lang.Long" [:constant "23"]])))
  (is (= BigDecimal (type-check [:construct "java.math.BigDecimal" [:constant "23"]])))

  (is (= Long (type-check [:invoke-static-method "java.lang.Long" "getLong" [:constant "java.specification.version"]
                           [:constant 34]])))

  (is (= String (type-check [:invoke-instance-method [:constant 12] "toString"])))

  (is (= java.time.Month (type-check [:get-static-field "java.time.Month" "JULY"])))

  (is (= Long
         (type-check [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"])))
  )

(deftest eval-exp-test
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
  )

(deftest preservation-test
  (let [pres (fn [exp t]
               (is (= t (type-check exp)))
               (is (= t (class (eval-exp exp)))))]
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
