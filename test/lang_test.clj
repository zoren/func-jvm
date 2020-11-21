(ns lang-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [lang :refer [annotate-exp eval-annotated-exp annotated-type annotate-type]]
   [unify :refer [normalize renumber]]
   [antlr :refer [parse-csl-exp]]
   )
  )

(defn first! [s]
  (when-not (= (count s) 1) (throw (ex-info "expected one element" {:found s})))
  (first s))

(defn mk-error-lister []
  (let [errors-atom (atom [])]
    {:errors-atom errors-atom :error (fn error
                                       ([message] (error message {}))
                                       ([message args] (swap! errors-atom conj (assoc args :message message))
                                        nil))}))

(defn throw-error
  ([m] (throw-error m {}))
  ([m a] (throw (ex-info (name m) a))))

(defn at [t] (annotated-type ((annotate-type throw-error) t)))

(defn at-error [t]
  (let [{:keys [error errors-atom]} (mk-error-lister)]
    (annotated-type ((annotate-type error) t))
    (-> @errors-atom first :message)))

(deftest annotate-type-test
  (is (= :class-not-found (at-error ["NoSuchClass"])))
  (is (= :type-arity-mismatch (at-error ["java.lang.Number" ["java.lang.Number"]])))
  (is (= :class-not-found (at-error ["java.util.Collection" ["NoSuchClass"]])))
  (is (= :type-arity-mismatch (at-error ["java.util.Collection"])))

  (is (= [Number] (at ["java.lang.Number"])))
  (is (= [String] (at ["java.lang.String"])))
  (is (= [java.util.Collection [java.lang.Long]] (at ["java.util.Collection" ["java.lang.Long"]])))
  (is (= [java.lang.Iterable [java.lang.Long]] (at ["java.lang.Iterable" ["java.lang.Long"]])))
  (is (= [java.util.Map [java.lang.Long] [java.lang.String]]
         (at ["java.util.Map" ["java.lang.Long"] ["java.lang.String"]]))))

(defn unwrap-singleton [s]
  (if (= (count s) 1)
    (first s)
    s))

(defn t
  ([e] (t {} e))
  ([st e]
   (let [annotated-exp ((annotate-exp throw-error) st e)]
     (-> annotated-exp
         annotated-type
         normalize
         renumber
         unwrap-singleton))))

(defn t-error-list [symbol-table exp]
  (let [errors-atom (atom [])
        error (fn error
                ([message] (error message {}))
                ([message args] (swap! errors-atom conj (assoc args :message message))
                 nil))
        annotated-exp ((annotate-exp error) symbol-table exp)]
    {:errors @errors-atom :annotated-exp annotated-exp}))

(defn t-error
  ([e] (t-error {} e))
  ([st e]
   (let [{:keys [errors]} (t-error-list st e)]
     (when (empty? errors) (throw (ex-info "no error" {})))
     (-> errors
         first!
         :message))))

(defn pt
  ([s] (pt {} s))
  ([st s] (t st (-> s parse-csl-exp))))

(deftest parse-annotated-exp-test
  (testing "constant"
    (is (=  (pt "9223372036854775808")))

    (is (= Boolean (pt "true")))
    (is (= Boolean (pt "false")))
    (is (= Long (pt "0")))
    (is (= Long (pt "1")))
    (is (= Long (pt "123")))

    (is (= BigDecimal (pt "0.0")))
    (is (= BigDecimal (pt "1.23")))
    (is (= BigDecimal (pt "12.46")))
    (is (= BigDecimal (pt "1e6")))
    (is (= BigDecimal (pt "1E10")))
    (is (= BigDecimal (pt "1E-3")))
    (is (= BigDecimal (pt "1.234E3")))

    #_(is (=  (pt "5a")))
    (is (= String (pt "\"\"")))
    (is (= java.time.Instant (pt "#2020-01-01T00:00:00Z#")))
    (is (= java.time.Duration (pt "#P1DT2H3M4S#")))
    (is (= java.time.Duration (pt "#-PT42.314S#")))
    (is (= {:clj-antlr/position {:row 0 :column 0 :index 0}
            :type [Long]}
           (meta ((annotate-exp throw-error) {} (parse-csl-exp "5"))))))

  (testing "variable"
    (is (= String (pt {"x" [String]} "x")))
    (is (= Long (pt {["M" "x"] [Long]} "M::x"))))

  (testing "if"
    (is (= Long (pt "if (true) 3 else 5"))))

  (testing "upcast"
    (is (= Number (pt "4 :> java.lang.Number")))
    (is (= Number (pt "4.5 :> java.lang.Number"))))

  (testing "lambda"
    (is (= [java.util.function.Function :a :a] (pt "\\x -> x")))
    (is (= [java.util.function.Function :a [Long]] (pt "\\x -> 5")))
    (is (= [java.util.function.Function [Long] [Long]] (pt "\\x -> if (true) x else 5")))
    (is (= [java.util.function.Function :a [java.util.function.Function :b :a]]
           (pt "\\x -> \\y -> x"))))
  )

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
    (is (= :upcast-invalid (t-error [:upcast [:constant 5] ["java.lang.String"]])))
    (is (= :upcast-invalid (t-error [:upcast [:constant ""] ["java.lang.Number"]])))
    (is (= :upcast-invalid (t-error [:upcast [:constant (Object.)] ["java.lang.Number"]])))
    (is (= :upcast-invalid (t-error [:upcast [:upcast [:constant 3] ["java.lang.Number"]] ["java.lang.Long"]])))
    (is (= :upcast-invalid (t-error [:upcast [:upcast [:constant 3] ["java.lang.Number"]] ["java.lang.Long"]])))
    (is (= :upcast-invalid
           (t-error [:upcast [:invoke-static-method "java.util.List" "of" [:constant 42]]
                     ["java.util.List" ["java.lang.String"]]])))

    (is (= Number (t [:upcast [:constant 3] ["java.lang.Number"]])))
    (is (= Number (t [:upcast [:constant 3.0] ["java.lang.Number"]])))
    (is (= Number (t [:if [:constant true]
                      [:upcast [:constant 3] ["java.lang.Number"]]
                      [:upcast [:constant 3.0] ["java.lang.Number"]]])))
    (is (= [java.util.Collection [Long]]
           (t [:upcast [:invoke-static-method "java.util.List" "of" [:constant 42]]
               ["java.util.Collection" ["java.lang.Long"]]])))
    (is (= [java.lang.Iterable [Long]]
           (t [:upcast [:invoke-static-method "java.util.List" "of" [:constant 42]]
               ["java.lang.Iterable" ["java.lang.Long"]]])))
    )

  (testing "function"
    (is (= [java.util.function.Function :a [Long]]
           (t [:function [:pattern-identifier "x"] [:constant 4]])))
    (is (= [java.util.function.Function :a :a]
           (t [:function [:pattern-identifier "x"] [:variable "x"]]))))

  (testing "function call"
    (is (= :argument-type-no-match (t-error [:invoke-function [:constant 5.0] [:constant 5.0]])))
    (is (= :argument-type-no-match
           (t-error [:invoke-function
                     [:function [:pattern-identifier "x"]
                      [:if [:constant true] [:constant 3] [:variable "x"]]]
                     [:constant 5.0]])))

    (is (= Long (t [:invoke-function [:function [:pattern-identifier "x"] [:variable "x"]] [:constant 5]])))))

(defn eval-exp
  ([exp] (eval-exp {} exp))
  ([st-env exp]
   (let [st (into {} (map (fn [[variable [t _value]]] [variable t]) st-env))
         annotated-exp ((annotate-exp throw-error) st exp)
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
                      [:upcast [:constant 3] ["java.lang.Number"]]
                      [:upcast [:constant 3.0] ["java.lang.Number"]]])))
  (is (= 3.0 (eval-exp [:if [:constant false]
                        [:upcast [:constant 3] ["java.lang.Number"]]
                        [:upcast [:constant 3.0] ["java.lang.Number"]]])))
  (is (= 5.0 (.apply (eval-exp [:function [:pattern-identifier "x"] [:variable "x"]]) 5.0)))

  (is (= 5.0 (eval-exp [:invoke-function [:function [:pattern-identifier "x"] [:variable "x"]] [:constant 5.0]])))
  )
