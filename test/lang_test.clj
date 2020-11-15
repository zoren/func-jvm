(ns lang-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [lang :refer [annotate-exp eval-annotated-exp annotated-type]]
   [unify :refer [normalize renumber]]
   )
  )

(defn unwrap-singleton [s]
  (if (= (count s) 1)
    (first s)
    s))

(defn t
  ([e] (t {} e))
  ([st e]
   (let [{:keys [annotated-exp errors]} ((annotate-exp st) e)]
     (when-not (empty? errors) (throw (ex-info "there where errors" {:errors errors})))
     (-> annotated-exp
         annotated-type
         normalize
         renumber
         unwrap-singleton))))

(defn first! [s]
  (when-not (= (count s) 1) (throw (ex-info "expected one element" {:found s})))
  (first s))

(defn t-error
  ([e] (t-error {} e))
  ([st e]
   (let [{:keys [errors]} ((annotate-exp st) e)]
     (when (empty? errors) (throw (ex-info "no error" {})))
     (-> errors
         first!
         :message))))

(deftest annotate-exp-test
  (testing "constant"
    (is (= Boolean (t [:constant false])))
    (is (= Boolean (t [:constant true])))
    (is (= String (t [:constant "abc"])))
    (is (= Long (t [:constant 123]))))

  (testing "class expression"
    (is (= :class-not-found (t-error [:class "NoSuchClass"])))
    (is (= Class (t [:class "java.lang.Long"]))))

  (testing "construct"
    (is (= :class-not-found
           (t-error [:construct "NoSuchClass"])))
    (is (= :constructor-not-found
           (t-error [:construct "java.lang.Long" [:constant 34]])))
    (is (= Object (t [:construct "java.lang.Object"])))
    (is (= Long (t [:construct "java.lang.Long" [:constant "23"]])))
    (is (= BigDecimal (t [:construct "java.math.BigDecimal" [:constant "23"]]))))

  (testing "static field"
    (is (= :class-not-found
           (t-error [:get-static-field "NoSuchClass" "noSuchField"])))
    (is (= :field-not-found
           (t-error [:get-static-field "java.time.Month" "noSuchField"])))
    (is (= :not-static
           (t-error [:get-static-field "experimentation.java.PublicInstanceField" "x"])))
    (is (= java.time.Month (t [:get-static-field "java.time.Month" "JULY"])))
    (is (= [java.util.function.Function [Long] [Long]]
           (t [:get-static-field "experimentation.java.PublicInstanceField" "longId"]))))

  (testing "instance field"
    (is (= :static
           (t-error [:get-instance-field [:get-static-field "java.time.Month" "JULY"] "JULY"])))
    (is (= :field-not-found
           (t-error [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "noSuchField"])))
    (is (= Long (t [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"])))
    (is (= [java.util.function.Function [Long] [Long]]
           (t [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "instanceFuncField"]))))

  (testing "static method invocation"
    (is (= :class-not-found
           (t-error [:invoke-static-method "NoSuchClass" "methodName"])))
    (is (= :method-not-found
           (t-error [:invoke-static-method "java.lang.Long" "noSuchMethod"])))
    (is (= :not-static
           (t-error [:invoke-static-method "java.lang.Long" "toString"])))
    (is (= :ambiguous-method-signature
           (t-error [:invoke-static-method "java.lang.Long" "getLong" [:constant "java.specification.version"]
                     [:constant 34]])))
    (is (= :argument-type-no-match
           (t-error [:invoke-static-method "java.util.List" "of" [:constant 42] [:constant "abc"]])))

    ;; uses primitive type
    (is (= String (t [:invoke-static-method "java.lang.Long" "toHexString" [:constant 42]])))
    (is (= Long (t [:invoke-static-method "java.lang.Long" "valueOf" [:constant "14"]])))
    (is (= [java.util.List :a] (t [:invoke-static-method "java.util.List" "of"])))
    (is (= [java.util.List [Long]] (t [:invoke-static-method "java.util.List" "of" [:constant 42]])))
    (is (= [java.util.List [Long]] (t [:invoke-static-method "java.util.List" "of" [:constant 5] [:constant 6]]))))

  (testing "instance method invocation"
    (is (= :method-not-found (t-error [:invoke-instance-method [:constant 12] "noSuchMethod"])))
    (is (= :static (t-error [:invoke-instance-method [:constant 12] "toHexString" [:constant 42]])))
    (is (= :argument-type-no-match (t-error [:invoke-instance-method [:constant 12] "compareTo" [:constant ""]])))

    (is (= String (t [:invoke-instance-method [:constant 12] "toString"])))
    (is (= Integer (t [:invoke-instance-method [:constant 12] "compareTo" [:constant 12]]))))

  (testing "if"
    (is (= :if-cond-not-boolean (t-error [:if [:constant "not bool"] [:constant 3] [:constant 5]])))
    (is (= :if-branches-differ (t-error [:if [:constant true] [:constant 3] [:constant "not same type as 3"]])))
    (is (= :if-branches-differ
           (t-error [:if [:constant true]
                     [:invoke-static-method "java.util.List" "of" [:constant 42]]
                     [:invoke-static-method "java.util.List" "of" [:constant 42.0]]])))

    (is (= Long (t [:if [:constant true] [:constant 3] [:constant 5]])))
    (is (= [java.util.List [Long]]
           (t [:if [:constant true]
               [:invoke-static-method "java.util.List" "of" [:constant 42]]
               [:invoke-static-method "java.util.List" "of" [:constant 123]]])))
    (is (= [java.util.List [Long]]
           (t [:if [:constant true]
               [:invoke-static-method "java.util.List" "of"]
               [:invoke-static-method "java.util.List" "of" [:constant 123]]])))
    (is (= [java.util.List :a]
           (t [:if [:constant true]
               [:invoke-static-method "java.util.List" "of"]
               [:invoke-static-method "java.util.List" "of"]]))))

  (testing "variable"
    (is (= :variable-not-found (t-error [:variable "noSuchVar"])))

    (is (= String (t {:x [String]} [:variable :x]))))

  (testing "upcast annotation"
    (is (= :upcast-invalid (t-error [:upcast [:constant 5] [String]])))
    (is (= :upcast-invalid (t-error [:upcast [:constant ""] [Number]])))
    (is (= :upcast-invalid (t-error [:upcast [:constant (Object.)] [Number]])))
    (is (= :upcast-invalid (t-error [:upcast [:upcast [:constant 3] [Number]] [Long]])))
    (is (= :upcast-invalid (t-error [:upcast [:upcast [:constant 3] [Number]] [Long]])))
    (is (= :upcast-invalid-type-arg
           (t-error [:upcast [:invoke-static-method "java.util.List" "of" [:constant 42]]
                     [java.util.List [String]]])))

    (is (= Number (t [:upcast [:constant 3] [Number]])))
    (is (= Number (t [:upcast [:constant 3.0] [Number]])))
    (is (= Number (t [:if [:constant true]
                      [:upcast [:constant 3] [Number]]
                      [:upcast [:constant 3.0] [Number]]])))
    (is (= [java.util.Collection [Long]]
           (t [:upcast [:invoke-static-method "java.util.List" "of" [:constant 42]]
               [java.util.Collection [Long]]])))
    (is (= [java.lang.Iterable [Long]]
           (t [:upcast [:invoke-static-method "java.util.List" "of" [:constant 42]]
               [java.lang.Iterable [Long]]])))
    )
  )

(defn eval-exp
  ([exp] (eval-exp {} exp))
  ([st-env exp]
   (let [st (into {} (map (fn [[variable [t _value]]] [variable t]) st-env))
         {:keys [annotated-exp errors]} ((annotate-exp st) exp)
         _ (when-not (empty? errors) (throw (ex-info "there were errors" {:errors errors})))
         env (into {} (map (fn [[variable [_t value]]] [variable value]) st-env))]
     (eval-annotated-exp env annotated-exp))))

(comment
  (annotate-exp [:invoke-static-method "java.util.List" "of" [:constant 42] [:constant "abc"]])
  (eval-exp [:invoke-instance-method [:get-static-field "java.lang.System" "out"] "println"
             [:constant "hello, world"]])
  )

(deftest eval-annotated-exp-test
  (is (= "abc" (eval-exp [:constant "abc"])))
  (is (= 123 (eval-exp [:constant 123])))

  (is (= Long (eval-exp [:class "java.lang.Long"])))

  (is (= Object (type (eval-exp [:construct "java.lang.Object"]))))
  (is (= "abc" (eval-exp [:construct "java.lang.String" [:constant "abc"]])))
  (is (= 123 (eval-exp [:construct "java.lang.Long" [:constant "123"]])))

  (is (= java.time.Month/JULY (eval-exp [:get-static-field "java.time.Month" "JULY"])))

  (is (= 5
         (eval-exp [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"])))

  (is (= 14 (eval-exp [:invoke-static-method "java.lang.Long" "valueOf" [:constant "14"]])))

  (is (= "123" (eval-exp [:invoke-instance-method [:constant 123] "toString"])))
  (is (= 6 (eval-exp [:invoke-instance-method [:construct "experimentation.java.PublicInstanceField"] "plus"])))
  (is (= "abc" (eval-exp [:invoke-instance-method
                          [:invoke-instance-method
                           [:construct "java.lang.StringBuilder" [:constant "a"]] "append" [:constant "bc"]
                           ] "toString"])))

  (is (= 3 (eval-exp [:if [:constant true] [:constant 3] [:constant 5]])))
  (is (= 5 (eval-exp [:if [:constant false] [:constant 3] [:constant 5]])))

  (is (= "abc" (eval-exp {:x [String "abc"]} [:variable :x])))

  (is (= 3 (eval-exp [:if [:constant true]
                      [:upcast [:constant 3] [Number]]
                      [:upcast [:constant 3.0] [Number]]])))
  (is (= 3.0 (eval-exp [:if [:constant false]
                        [:upcast [:constant 3] [Number]]
                        [:upcast [:constant 3.0] [Number]]]))))
