(ns annotate-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [annotate :refer [annotate-exp annotated-type annotate-type annotate-top-level-decls]]
   [unify :refer [normalize renumber]]
   [antlr :refer [parse-exp parse-type]]
   )
  (:import
   [java.util.function Function])
  )

(defn first! [s]
  (when-not (= (count s) 1) (throw (ex-info "expected one element" {:found s})))
  (first s))

(defn unwrap-singleton [s]
  (if (and (coll? s) (= (count s) 1))
    (first s)
    s))

(defn run-errors [f]
  (let [errors-atom (atom [])
        v (binding
              [annotate/*report-error
               (fn [message] (swap! errors-atom conj message))]
            (f))]
    [@errors-atom v]))

(defn at [s]
  (let [[errors v] (run-errors #(annotate-type (parse-type s)))]
    (when-not (empty? errors) (throw (ex-info "errors" {:errors errors})))
    (-> v annotated-type unwrap-singleton)))

(defn at-error [t]
  (let [[errors _] (run-errors #(-> t
                                    parse-type
                                    annotate-type))]
    (when (empty? errors) (throw (ex-info "no error" {})))
    (-> errors
        first!
        :message)))

(deftest type-test
  (is (= :type-not-found (at-error "NoSuchType")))
  (is (= :type-arity-mismatch (at-error "java::util::List")))
  (is (= :type-arity-mismatch (at-error "java::util::List java::lang::Number java::lang::String")))
  (is (= :tuple-cannot-have-arity-one (at-error "Tuple java::lang::Long")))
  (is (= :type-not-found (at-error "java::util::List NoSuch"))) ; make sure we get a List Object here

  (is (= Long (at "java::lang::Long")))
  (is (= [java.util.List [Long]] (at "java::util::List java::lang::Long")))
  (is (= [java.util.Map [Number] [String]] (at "java::util::Map java::lang::Number java::lang::String"))))

(defn pt
  ([s] (pt {} s))
  ([st s]
   (let [[errors annotated-exp] (run-errors #(annotate-exp {:st st} (parse-exp s)))]
     (when-not (empty? errors) (throw (ex-info "annotate error" {:error errors})))
     (-> annotated-exp
         annotated-type
         normalize
         renumber
         unwrap-singleton))))

(defn pe
  ([s] (pe {} s))
  ([st s]
   (let [[errors _] (run-errors #(annotate-exp {:st st} (parse-exp s)))]
     (when (empty? errors) (throw (ex-info "no error" {})))
     (-> errors
         first!
         :message))))

(deftest parse-annotated-exp-test
  (testing "constant"
    (is (= Boolean (pt "false")))
    (is (= Boolean (pt "true")))

    #_    (is (=  (pt "9223372036854775808"))) ;; error then number is larger than
    (is (= Long (pt "0")))
    (is (= Long (pt "1")))
    (is (= Long (pt "123")))
    (is (= {:clj-antlr/position {:row 0 :column 0 :index 0}
            :type [Long]}
           (meta (annotate-exp {} (parse-exp "5")))))

    (is (= BigDecimal (pt "0.0")))
    (is (= BigDecimal (pt "1.23")))
    (is (= BigDecimal (pt "12.46")))
    (is (= BigDecimal (pt "1e6")))
    (is (= BigDecimal (pt "1E10")))
    (is (= BigDecimal (pt "1E-3")))
    (is (= BigDecimal (pt "1.234E3")))

    #_(is (=  (pt "5a")))
    (is (= String (pt "\"\"")))
    (is (= String (pt "\"abc\"")))

    (is (= java.time.Instant (pt "#2020-01-01T00:00:00Z#")))

    (is (= java.time.Duration (pt "#P1DT2H3M4S#")))
    (is (= java.time.Duration (pt "#-PT42.314S#")))
    )

  (testing "if"
    (is (= :if-cond-not-boolean (pe "if (1) 3 else 5")))
    (is (= :if-branches-differ (pe "if (true) 3 else 5.0")))

    (is (= Long (pt "if (true) 3 else 5"))))

  (testing "variable"
    (is (= :variable-not-found (pe "noSuchVar")))

    (is (= String (pt {"x" {:t [String]}} "x")))
    (is (= Long (pt {["M" "x"] {:t [Long]}} "M::x"))))

  (testing "upcast"
    (is (= :upcast-invalid (pe "4 :> java::util::List java::lang::Number")))

    (is (= Number (pt "4 :> java::lang::Number")))
    (is (= Number (pt "4.5 :> java::lang::Number"))))

  (testing "tuple"
    (is (= Long (pt "(3)")))

    (is (= :tuple (pt "()")))
    (is (= [:tuple [Long] [BigDecimal]] (pt "(2, 3.0)")))
    (is (= [:tuple [Long] [Long] [Long]] (pt "(2, 3, 4)")))
    ;; (is (= [:tuple [Long] [Long] (pt "\\(x, y) : Tuple java::lang::Long java::lang::Long -> x + y")))
    ;; (is (= [:tuple [Long] [Long] [Long]] (pt "\\(x, y) : Tuple java::lang::Long java::lang::Long -> x + y")))
    (is (= [Function [Long] [Long]] (pt "\\x : java::lang::Long -> x + 7")))
    (is (= [Function [:tuple [Long] [Long]] [Long]]
           (pt "\\(x, y) : Tuple java::lang::Long java::lang::Long -> x"))))

  (testing "lambda"
    (is (= :argument-pattern-types-differ (pe "\\1 -> 2 | 1.0 -> 2")))
    (is (= :body-types-differ (pe "\\ 1 -> 2 | 1 -> 2.0")))

    (is (= [java.util.function.Function :a :a] (pt "\\x -> x")))
    (is (= [java.util.function.Function :a [Long]] (pt "\\x -> 5")))
    (is (= [java.util.function.Function [Long] [Long]] (pt "\\x -> if (true) x else 5")))
    (is (= [java.util.function.Function :a [java.util.function.Function :b :a]]
           (pt "\\x -> \\y -> x")))
    (is (= [java.util.function.Function [experimentation.java.PublicInstanceField] [Long]]
           (pt "\\(o : experimentation::java::PublicInstanceField) -> o.x")))
    (is (= [java.util.function.Function [experimentation.java.PublicInstanceField] [experimentation.java.PublicInstanceField]]
           (pt "\\(o : experimentation::java::PublicInstanceField) -> o")))
    #_(is (= "fail because function parameters have monomorphic type"
             (pt "\\f -> if(true) f 0 else f 0.0")))
    #_((fn [f] (if true (f 0) (f 0.0))) identity)
    (is (= [java.util.function.Function [Long] [Long]]
           (pt "\\5 -> 6")))
    (is (= [java.util.function.Function [Long] [Long]]
           (pt "\\ _ -> 2 | x -> x")))
    (is (= Long (pt "(\\ (x, y) -> x) (2, 3.0)")))
    )

  (testing "parens"
    (is (= Long (pt "5")))
    (is (= [java.util.function.Function :a :a] (pt "(\\x -> x)"))))

  (testing "function apply"
    (is (= Long (pt "(\\x -> x) 5")))
    (is (= Long (pt "(\\(o : experimentation::java::PublicInstanceField) -> o.x)
                         (experimentation::java::PublicInstanceField (5, 6))")
           )))

  (testing "constructor"
    (is (= Long (pt "java::lang::Long \"23\"")))
    ;;    (is (= Long (pt "java::lang::Long 56.6"))) ; reports error
    (is (= Long (pt "java::lang::Long (\"23\")")))
    ;;(is (= Long (pt "java::lang::Long (123)")))
    (is (= experimentation.java.PublicInstanceField (pt "experimentation::java::PublicInstanceField (3, 42)"))))

  (testing "static field"
    (is (= :field-not-found (pe "java::time::Month.MARIL")))

    (is (= java.time.Month (pt "java::time::Month.JULY")))
    )

  (testing "instance field"
    (is (= :field-not-found (pe "(experimentation::java::PublicInstanceField(5, 6)).noSuchField")))

    (is (= Long (pt "(experimentation::java::PublicInstanceField(5, 6)).x")))
    (is (= [java.util.function.Function [experimentation.java.PublicInstanceField] [Long]]
           (pt "\\(o : experimentation::java::PublicInstanceField) -> o.x")))
    ;; todo
    #_(is (= Long (pt "(experimentation::java::PublicInstanceField(5, 6)).x.y")))
    )

  (testing "static method"
    (is (= :method-not-found (pe "java::util::List.noSuchMethod 42")))
    (is (= :no-method-overload-found (pe "java::lang::Long.getLong (4, 4)")))

    (is (= [java.util.List [Long]] (pt "java::util::List.of 42")))
    (is (= [java.util.List [Long]] (pt "java::util::List.of (5, 6)"))))

  (testing "instance method"
    (is (= :method-not-found (pe "23.noSuchMethod ()")))
    (is (= :no-method-overload-found (pe "java::lang::StringBuilder(\"a\").insert(1, 2)")))

    (is (= String (pt "12.toString()"))))

  (testing "let"
    (is (= Long (pt "let val x = 5 in 6")))
    (is (= Long (pt "let val x = 5 in x")))
    (is (= Long (pt "let val x = 5 val y = x in y")))
    (is (= Long (pt "let val f = \\x -> x in f 3")))
    (is (= Long (pt "let val f = \\x -> x val y = f 1 in y")))
    (is (= Long (pt "let val f = \\x -> x val y = f 1 val z = f 1 in z")))
    (is (= Long (pt "let val f = \\x -> 5 in if(true) f 1.0 else f 1")))
    (is (= Long (pt "let val f = \\x -> x val y = f 1.0 val z = f 1 in z")))
    )

  (testing "unary minus"
    (is (= :unary-minus-not-supported-on-type (pe "- \"\"")))

    (is (= Long (pt "- 5")))
    (is (= BigDecimal (pt "- 5.0")))
    )

  (testing "arithmetic operator"
    (is (= :operand-types-differ (pe "true + 2")))
    (is (= :operand-types-differ (pe "1.0 + 2")))
    (is (= :operand-types-differ (pe "5 + 6.0")))
    (is (= :operand-types-differ (pe "3 + \"d\"")))
    (is (= :no-overload-found (pe "\"abc\" + \"d\"")))
    (is (= :no-overload-found (pe "true + false")))

    (is (= Long (pt "5 + 6")))
    (is (= BigDecimal (pt "5.0 + 6.0")))
    (is (= BigDecimal (pt "5.0 - 6.0")))
    (is (= BigDecimal (pt "5.0 * 6.0")))
    (is (= BigDecimal (pt "5.0 / 6.0")))
    )

  (testing "comparison operator"
    (is (= :operand-types-differ (pe "true < 1")))
    (is (= :operand-types-differ (pe "1 < 1.0")))
    (is (= :comparison-does-not-apply (pe "\"abc\" <= \"d\"")))

    (is (= Boolean (pt "5 < 6")))
    (is (= Boolean (pt "5.0 < 6.0")))
    #_(is (= Boolean (pt "#2020-01-01T00:00:00Z# < #2020-01-01T00:00:00Z#")))
    )

  (testing "logic operator"
    (is (= :operand-types-differ (pe "true && 1")))
    (is (= :operand-types-differ (pe "1 && 1.0")))
    (is (= :logical-operator-on-non-boolean (pe "\"abc\" && \"d\"")))

    (is (= Boolean (pt "true && false")))
    (is (= Boolean (pt "true || false")))))

(defn atlds [s]
  (let [[errors v] (run-errors #(annotate-top-level-decls {} (antlr/parse-top-level s)))]
    (when-not (empty? errors) (throw (ex-info "errors" {:errors errors})))
    v))

(defn atlds-errs [s]
  (let [[errors _v] (run-errors #(annotate-top-level-decls {} (antlr/parse-top-level s)))]
    (when (empty? errors) (throw (ex-info "no errors" {})))
    errors))

(defn atld-err [s] (-> s atlds-errs first! :message))

(deftest toplevel
  (is (= :variable-not-found (atld-err "val _ = x")))
  (is (= :variable-not-found (atld-err "val _ = x val x = 5")))

  (atlds "val x = 5")
  (atlds "val x = 5 val y = x")
  )

