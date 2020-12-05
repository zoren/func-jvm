(ns annotate-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [annotate :refer [annotate-exp annotated-type annotate-type]]
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

(defn unwrap-singleton [s]
  (if (and (vector? s) (= (count s) 1))
    (first s)
    s))

(defn t
  ([e] (t {} e))
  ([st e]
   (let [_ (annotate/reset-errors!)
         annotated-exp (annotate-exp {:st st} e)
         _ (when-not (empty? @annotate/errors) (throw (ex-info "annotate error" {:error @annotate/errors})))]
     (-> annotated-exp
         annotated-type
         normalize
         renumber
         unwrap-singleton))))

(defn t-error-list [symbol-table exp]
  (let [errors-atom (atom [])
        annotated-exp (annotate-exp symbol-table exp)]
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
    (is (= Boolean (pt "true")))
    (is (= Boolean (pt "false")))

    #_    (is (=  (pt "9223372036854775808"))) ;; error then number is larger than
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
    (is (= String (pt "\"abc\"")))

    (is (= java.time.Instant (pt "#2020-01-01T00:00:00Z#")))

    (is (= java.time.Duration (pt "#P1DT2H3M4S#")))
    (is (= java.time.Duration (pt "#-PT42.314S#")))
    (is (= {:clj-antlr/position {:row 0 :column 0 :index 0}
            :type [Long]}
           (meta (annotate-exp {} (parse-csl-exp "5"))))))

  (testing "variable"
    (is (= String (pt {"x" {:t [String]}} "x")))
    (is (= Long (pt {["M" "x"] {:t [Long]}} "M::x"))))

  (testing "if"
    (is (= Long (pt "if (true) 3 else 5"))))

  (testing "upcast"
    (is (= Number (pt "4 :> java::lang::Number")))
    (is (= Number (pt "4.5 :> java::lang::Number"))))

  (testing "lambda"
    (is (= [java.util.function.Function :a :a] (pt "\\x -> x")))
    (is (= [java.util.function.Function :a [Long]] (pt "\\x -> 5")))
    (is (= [java.util.function.Function [Long] [Long]] (pt "\\x -> if (true) x else 5")))
    (is (= [java.util.function.Function :a [java.util.function.Function :b :a]]
           (pt "\\x -> \\y -> x")))
    (is (= [java.util.function.Function [experimentation.java.PublicInstanceField] [Long]]
           (pt "\\(o : experimentation::java::PublicInstanceField) -> o.x")))
    (is (= [java.util.function.Function [experimentation.java.PublicInstanceField] [experimentation.java.PublicInstanceField]]
           (pt "\\(o : experimentation::java::PublicInstanceField) -> o"))))

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
    (is (= java.time.Month (pt "java::time::Month.JULY")))
    )

  (testing "instance field"
    (is (= Long (pt "(experimentation::java::PublicInstanceField(5, 6)).x")))
    (is (= [java.util.function.Function [experimentation.java.PublicInstanceField] [Long]]
           (pt "\\(o : experimentation::java::PublicInstanceField) -> o.x"))))

  (testing "static method"
    (is (= [java.util.List [Long]] (pt "java::util::List.of 42")))
    (is (= [java.util.List [Long]] (pt "java::util::List.of (5, 6)"))))

  (testing "instance method"
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

  (testing "arithmetic operator"
    (is (= Long (pt "5 + 6")))
    (is (= BigDecimal (pt "5.0 + 6.0")))
    #_(is (= BigDecimal (pt "5 + 6.0")))
    (is (= BigDecimal (pt "5.0 - 6.0")))
    (is (= BigDecimal (pt "5.0 * 6.0")))
    (is (= BigDecimal (pt "5.0 / 6.0")))
    )

  (testing "comparison operator"
    (is (= Boolean (pt "5 < 6")))
    (is (= Boolean (pt "5.0 < 6.0")))
    (is (= Boolean (pt "#2020-01-01T00:00:00Z# < #2020-01-01T00:00:00Z#")))
    )

  (testing "logic operator"
    (is (= Boolean (pt "true && false")))
    (is (= Boolean (pt "true || false")))
    )
  )

