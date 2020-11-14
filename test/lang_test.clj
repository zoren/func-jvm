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
   (let [[annotated-exp errors] ((annotate-exp st) e)]
     (when-not (empty? errors) (throw (ex-info "there where errors" {})))
     (-> annotated-exp
         annotated-type
         normalize
         renumber
         unwrap-singleton
                                        ;       first
         )
     )))

(defn t-errors
  ([e] (t-errors {} e))
  ([st e]
   (let [[_annotated-exp errors] ((annotate-exp st) e)]
     (when (empty? errors) (throw (ex-info "no error" {})))
     (-> errors
         unwrap-singleton)
     )))

(deftest annotate-test
  (testing "constant"
    (is (= Boolean (t [:constant false])))
    (is (= Boolean (t [:constant true])))
    (is (= String (t [:constant "abc"])))
    (is (= Long (t [:constant 123]))))

  (testing "class expression"
    (is (= :class-not-found (:message (t-errors [:class "NoSuchClass"]))))
    (is (= Class (t [:class "java.lang.Long"]))))

  (testing "construct"
    (is (= :class-not-found
           (:message (t-errors [:construct "NoSuchClass"]))))
    (is (= :constructor-not-found
           (:message (t-errors [:construct "java.lang.Long" [:constant 34]]))))
    (is (= Object (t [:construct "java.lang.Object"])))
    (is (= Long (t [:construct "java.lang.Long" [:constant "23"]])))
    (is (= BigDecimal (t [:construct "java.math.BigDecimal" [:constant "23"]]))))

  (testing "static field"
    (is (= :class-not-found
           (:message (t-errors [:get-static-field "NoSuchClass" "noSuchField"]))))
    (is (= :field-not-found
           (:message (t-errors [:get-static-field "java.time.Month" "noSuchField"]))))
    (is (= :not-static
           (:message (t-errors [:get-static-field "experimentation.java.PublicInstanceField" "x"]))))
    (is (= java.time.Month (t [:get-static-field "java.time.Month" "JULY"]))))

  ;; (is (thrown? clojure.lang.ExceptionInfo #"ambiguous"
  ;;              (t [:invoke-static-method "java.lang.Long" "getLong" [:constant "java.specification.version"]
  ;;                  [:constant 34]])))
  ;; ;; uses primitive type
  ;; (is (= String (t [:invoke-static-method "java.lang.Long" "toHexString" [:constant 42]])))
  ;; (is (= [java.util.List :a] (t [:invoke-static-method "java.util.List" "of"])))
  ;; (is (= [java.util.List [Long]] (t [:invoke-static-method "java.util.List" "of" [:constant 42]])))
  ;; (is (= [java.util.List [Long]] (t [:invoke-static-method "java.util.List" "of" [:constant 5] [:constant 6]])))
  ;; (is (thrown? clojure.lang.ExceptionInfo #"differ"
  ;;              (t [:invoke-static-method "java.util.List" "of" [:constant 42] [:constant "abc"]])))

  ;; (is (= String (t [:invoke-instance-method [:constant 12] "toString"])))
  ;; (is (= Integer (t [:invoke-instance-method [:constant 12] "compareTo" [:constant 12]])))

  ;; (is (= Long (t [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"])))

  ;; (is (thrown? clojure.lang.ExceptionInfo (t [:if [:constant "not bool"] [:constant 3] [:constant 5]])))
  ;; (is (thrown? clojure.lang.ExceptionInfo (t [:if [:constant true] [:constant 3] [:constant "not same type as 3"]])))
  ;; (is (= Long (t [:if [:constant true] [:constant 3] [:constant 5]])))

  ;; (is (= String (t {:x String} [:var :x])))

  ;; (is (thrown? clojure.lang.ExceptionInfo #"upcast invalid" (t [:upcast [:constant 5] String])))
  ;; (is (thrown? clojure.lang.ExceptionInfo #"upcast invalid" (t [:upcast [:constant ""] Number])))
  ;; (is (thrown? clojure.lang.ExceptionInfo #"upcast invalid" (t [:upcast [:constant (Object.)] Number])))
  ;; (is (= Number (t [:upcast [:constant 3] Number])))
  ;; (is (= Number (t [:if [:constant true]
  ;;                   [:upcast [:constant 3] Number]
  ;;                   [:upcast [:constant 3.0] Number]])))
  )

;; (defn eval-exp
;;   ([exp] (eval-exp {} exp))
;;   ([st-env exp] (eval-annotated-exp
;;                  (into {} (map (fn [[variable [_t value]]] [variable value]) st-env))
;;                  (annotate-exp (into {} (map (fn [[variable [t _value]]] [variable t]) st-env)) exp))))

;; (comment
;;   (annotate-exp [:invoke-static-method "java.util.List" "of" [:constant 42] [:constant "abc"]])
;;   (eval-exp [:invoke-instance-method [:get-static-field "java.lang.System" "out"] "println" [:constant "hello, world"]])
;;   (java.util.List/of 2 "")
;;   )

;; (deftest eval-annotated-exp-test
;;   (is (= "abc" (eval-exp [:constant "abc"])))
;;   (is (= 123 (eval-exp [:constant 123])))

;;   (is (= Long (eval-exp [:class "java.lang.Long"])))

;;   (is (= Object (type (eval-exp [:construct "java.lang.Object"]))))
;;   (is (= "abc" (eval-exp [:construct "java.lang.String" [:constant "abc"]])))
;;   (is (= 123 (eval-exp [:construct "java.lang.Long" [:constant "123"]])))

;;   (is (= 14 (eval-exp [:invoke-static-method "java.lang.Long" "valueOf" [:constant "14"]])))

;;   (is (= "123" (eval-exp [:invoke-instance-method [:constant 123] "toString"])))
;;   (is (= 6 (eval-exp [:invoke-instance-method [:construct "experimentation.java.PublicInstanceField"] "plus"])))
;;   (is (= "abc" (eval-exp [:invoke-instance-method
;;                           [:invoke-instance-method
;;                            [:construct "java.lang.StringBuilder" [:constant "a"]] "append" [:constant "bc"]
;;                            ] "toString"])))

;;   (is (= java.time.Month/JULY (eval-exp [:get-static-field "java.time.Month" "JULY"])))

;;   (is (= 5
;;          (eval-exp [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"])))

;;   (is (= 3 (eval-exp [:if [:constant true] [:constant 3] [:constant 5]])))
;;   (is (= 5 (eval-exp [:if [:constant false] [:constant 3] [:constant 5]])))

;;   (is (thrown? clojure.lang.ExceptionInfo #"variable not found" (eval-exp [:var :x])))
;;   (is (= "abc" (eval-exp {:x [String "abc"]} [:var :x])))

;;   (is (= 3 (eval-exp [:if [:constant true]
;;                       [:upcast [:constant 3] Number]
;;                       [:upcast [:constant 3.0] Number]])))
;;   (is (= 3.0 (eval-exp [:if [:constant false]
;;                         [:upcast [:constant 3] Number]
;;                         [:upcast [:constant 3.0] Number]]))))

;; (deftest preservation-test
;;   (let [pres (fn pres
;;                ([exp t]
;;                 (pres {} exp t))
;;                ([st-env exp t]
;;                 (let [annotated-exp (annotate-exp
;;                                      (into {} (map (fn [[variable [t _value]]] [variable t]) st-env)) exp)]
;;                   (is (= t (annotated-type annotated-exp)))
;;                   (is (= t (class (eval-annotated-exp
;;                                    (into {} (map (fn [[variable [_t value]]] [variable value]) st-env)) annotated-exp)))))))]
;;     (pres [:constant "abc"] String)
;;     (pres [:constant 123] Long)

;;     (pres [:class "java.lang.Long"] Class)

;;     (pres [:construct "java.lang.Object"] Object)
;;     (pres [:construct "java.lang.Long" [:constant "123"]] Long)
;;     (pres [:construct "java.math.BigDecimal" [:constant "123"]] BigDecimal)

;;     (pres [:invoke-static-method "java.lang.Long" "valueOf" [:constant "14"]] Long)

;;     (pres [:invoke-instance-method [:constant 12] "toString"] String)

;;     (pres [:get-static-field "java.time.Month" "JULY"] java.time.Month)

;;     (pres [:get-instance-field [:construct "experimentation.java.PublicInstanceField"] "x"] Long)

;;     (pres {:x [Long 42]} [:var :x] Long)))
