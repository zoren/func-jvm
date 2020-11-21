(ns antlr
  (:require
   [clj-antlr.core :as a]))

(defn try-parse-number [s]
  (try
    (Long. s)
    (catch NumberFormatException _nfe
      )))

(def trim-start-end #(subs % 1 (-> % count dec)))

(defn skip-odd [s]
  (if (seq s)
    (conj (skip-odd (rest (rest s))) (first s))
    ()))

(defn string->value [[first-elem :as s]]
  (case first-elem
    \" (trim-start-end s)
    \# (let [content (trim-start-end s)]
         (case (first content)
           \P (java.time.Duration/parse content)
           \- (java.time.Duration/parse content)
           (java.time.Instant/parse content)))
    :float (BigDecimal. (second s))
    :integer (Long/parseLong (second s)) ; todo give gracefull error message
    (case s
      "true" Boolean/TRUE
      "false" Boolean/FALSE
      (throw (ex-info "unknown constant" {:s s})))))

(defn convert-qname [names-seps]
  (let [names (skip-odd names-seps)]
    (if (= (count names) 1) (first names) (into [] names))))

(defn convert-type [[_ & t]]
  (cond
    (= (ffirst t) :java_qualified)
    (into [(-> t first second)] (map convert-type (rest t)))
    )
  )
(def antlr-parse-csl-type (a/parser "csl.g4" {:root "type_eof"}))
(-> "java.lang.A" antlr-parse-csl-type rest butlast first convert-type)
;;(-> "A B" antlr-parse-csl-type rest butlast first convert-type)
(type '(:type (:qualified_upper "I") (:type (:qualified_upper "A"))))

(defn convert-pattern [[_pattern [kind & args :as input]]]
  (case kind
    :pattern_identifier
    (let [[var] args]
      (with-meta [:pattern-identifier var] (meta input)))))

(defn- convert-csl-exp [[_ [kind & args :as input] :as total]]
  (case kind
    :constant
    (let [[s] args
          value (string->value s)]
      (with-meta [kind value] (meta input)))

    :variable
    (let [[[_ & names-seps]] args
          qname (convert-qname names-seps)]
      (with-meta [kind qname] (meta input)))

    :if
    (let [[_if _lpar cond _rpar then _else else] args]
      (with-meta [kind (convert-csl-exp cond) (convert-csl-exp then) (convert-csl-exp else)] (meta input)))

    :expression
    (case (count total)
      4
      (let [[_ t1 s t2] total]
        (case s
          ":>" [:upcast (convert-csl-exp t1) (convert-type t2)]
          ))
      )

    :lambda
    (let [[_backslash & cases] args]
      (when (< 1 (count cases)) (throw (ex-info "only one case supported for now" {:cases cases})))
      (prn cases)
      (let [case (first cases)
            [_ pat _arrow body] case
            cpat (convert-pattern pat)
            cbody (convert-csl-exp body)]
        (with-meta [:function cpat cbody] (meta input))))

    (throw (ex-info "convert-csl-exp: unknown exp type" {:args args :c (count input)}))
    ))

(def antlr-parse-csl-exp (a/parser "csl.g4" {:root "expression_eof"}))
(defn parse-csl-exp [s] (-> s antlr-parse-csl-exp second convert-csl-exp))

(comment
  (let [[_ e1 op e2] (-> "v :> I" antlr-parse-csl-exp rest butlast first)
        [kind & args :as input] (-> "v :> I" antlr-parse-csl-exp rest butlast first)]
    [e1 op e2]
    [kind args input]
    )
  (convert-csl-exp '(:expression (:variable (:qualified_lower "v"))))
  (-> "if (true) 2 else 3" parse-csl-exp)
  (-> "v" parse-csl-exp)
  (-> "v" antlr-parse-csl-exp second convert-csl-exp)
  (-> "3+2" antlr-parse-csl-exp)
  (-> "3*2" antlr-parse-csl-exp)
  (-> "5" parse-csl-exp)
  (-> "5e3" parse-csl-exp)
  (-> "true true" antlr-parse-csl-exp)
  (-> "M::v" parse-csl-exp)


  (clojure.pprint/pprint ((a/parser "csl.g4") "
val x = 2
val y = x")
                         )
  (clojure.pprint/pprint ((a/parser "csl.g4")
                          (slurp "/Users/zoren/deon/realstocks/csl-src/lifecycle.csl")))
  (clojure.pprint/pprint (csl "val (\"x\", 3, []) = [1,2,3]"))
  (clojure.pprint/pprint (csl "val x = \\s -> 2 + \"3\" * 4 <= 3 && 5 <= -x || #2020#"))

  (clojure.pprint/pprint (csl "val max = \\(x: Float) -> \\(y: Float) -> if (x < y) y else x"))
  (clojure.pprint/pprint (csl "val couponAmount = \\(terms: Terms) -> \\baseRate -> terms.loanSize * (terms.margin + max 0.0 baseRate)"))
  (clojure.pprint/pprint (csl "val c = x.f1.f2"))


  (->
   (csl "val x = 2 + 3 * 4 <= 3 && 5 <= -x")
   second
   second
                                        ;second
                                        ; second
                                        ; meta
   last
   meta
   )
  (System/getProperty "user.dir")

  (-> '(:source_text (:top_level_decl "val" (:pattern (:pattern_identifier "x")) "=" (:expression (:constant "2"))) (:top_level_decl "val" (:pattern (:pattern_identifier "y")) "=" (:expression (:qualified_lower "x"))) "<EOF>")
      rest
      butlast)
  ((annotate-top-level-decl #(throw (ex-info %1 %2)))
   {}
   '(:top_level_decl "val" (:pattern (:pattern_identifier "x")) "=" (:expression (:constant "2"))))
  ((annotate-source-text #(throw (ex-info (str %1) %2)))
   {}
   ((a/parser "csl.g4") "
val x = 2
val y = if (x) 4 else 3")
   )
  (get-proxy-class Object)
  (get-proxy-class Object)
  ;; (def o (reify (symbol "java.lang.Object")
  ;;          (toString [this] "hej")))
  ;; (str o)
  )

