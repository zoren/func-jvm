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

    :tuple_or_paren
    (let [[_lpar & elements-separators] (butlast args)
          elements (skip-odd elements-separators)]
      (when-not (= (count elements) 1) (throw (ex-info "tuples not yet implemented" {:elements elements})))
      (-> elements first convert-csl-exp))

    (throw (ex-info "convert-csl-exp: unknown exp type" {:args args :c (count input)}))
    ))

(def antlr-parse-csl-exp (a/parser "csl.g4" {:root "expression_eof"}))
(defn parse-csl-exp [s] (-> s antlr-parse-csl-exp second convert-csl-exp))

