(ns evaluate-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [annotate :refer [annotate-exp]]
   [antlr :refer [parse-csl-exp]]
   [evaluate :refer [eval-annotated-exp]]
   ))

(defn eval-exp [s]
  (eval-annotated-exp {} (annotate-exp {} (parse-csl-exp s))))

(deftest expression-test
  (testing "constants"
    (is (= false (eval-exp "false")))
    (is (= true (eval-exp "true")))
    (is (= 123 (eval-exp "123")))
    (is (= 5.0M (eval-exp "5.0")))
    (is (= "abc" (eval-exp "\"abc\"")))
    (is (= (java.time.Instant/parse "2020-01-01T00:00:00Z")
           (eval-exp "#2020-01-01T00:00:00Z#")))
    (is (= (java.time.Duration/parse "P1DT2H3M4S")
           (eval-exp "#P1DT2H3M4S#"))))

  (testing "constructor"
    (is (= Object (type (eval-exp "java::lang::Object()"))))
    (is (= "abc" (eval-exp "java::lang::String(\"abc\")")))
    (is (= 123 (eval-exp "java::lang::Long(\"123\")")))
    ;;(is (= 123 (eval-exp "java::lang::Long(123)"))) ; no such ctor
    )

  (testing "static field"
    (is (= java.time.Month/JULY (eval-exp "java::time::Month.JULY"))))

  (testing "instance field"
    (is (= 11
           (eval-exp "experimentation::java::PublicInstanceField(5, 6).x"))))

  (testing "static method call"
    (is (= 14 (eval-exp "java::lang::Long.valueOf 14"))))

  (testing "instace method call"
    (is (= "123" (eval-exp "123.toString()")))
    (is (= 12 (eval-exp "experimentation::java::PublicInstanceField(5, 6).plus()")))
    (is (= "abc" (eval-exp "java::lang::StringBuilder(\"a\").append(\"bc\").toString()"))))

  (testing "if"
    (is (= 3 (eval-exp "if (true) 3 else 5")))
    (is (= 5 (eval-exp "if (false) 3 else 5"))))

  (testing "unary minus"
    (is (= -5 (eval-exp "- 5")))
    (is (= 5 (eval-exp "- -5")))
    (is (= -5.0M (eval-exp "- 5.0")))
    (is (= 5.0M (eval-exp "- -5.0")))
    )

  (testing "binary"
    (is (= 11 (eval-exp "5 + 6")))
    (is (= -1 (eval-exp "5 - 6")))
    (is (= 30 (eval-exp "5 * 6")))
    (is (= 0 (eval-exp "5 / 6")))
    (is (= (binding [*math-context* java.math.MathContext/DECIMAL128]
             (/ 1.0M 3.0M)) (eval-exp "1.0 / 3.0")))
    (is (= true (eval-exp "5 <= 6")))
    (is (= true (eval-exp "5.0 <= 6.0")))
    #_(is (= true (eval-exp "#2020-01-01T00:00:00Z# < #2020-01-01T00:00:00Z#")))

    (is (= false (eval-exp "false && false")))
    (is (= false (eval-exp "false && true")))
    (is (= false (eval-exp "true && false")))
    (is (= true (eval-exp "true && true")))

    (is (= false (eval-exp "false || false")))
    (is (= true (eval-exp "false || true")))
    (is (= true (eval-exp "true || false")))
    (is (= true (eval-exp "true || true"))))

  (testing "upcast"
    (is (= 3 (eval-exp "if (true) 3 :> java::lang::Number else 3.0 :> java::lang::Number")))
    (is (= 3.0M (eval-exp "if (false) 3 :> java::lang::Number else 3.0 :> java::lang::Number"))))

  (testing "lambda"
    (is (= 5 (.apply (eval-exp "\\x->x") 5))))

  (testing "invoke function"
    (is (= 5 (eval-exp "(\\x -> x) 5")))
    (is (= 11 (eval-exp "(\\(o : experimentation::java::PublicInstanceField) -> o.x)
                         (experimentation::java::PublicInstanceField (5, 6))")))
    (is (= 5 (eval-exp "(\\_ -> 5) 6"))))

  (testing "let"
    (is (= 5 (eval-exp "let val x = 5 val y = x in y")))
    (is (= 5 (eval-exp "let val f = \\x -> 5 in if(true) f 1.0 else f 1")))
    (is (= 5 (eval-exp "let val f = \\x -> x val y = f 1.0 val z = f 5 in z")))))
