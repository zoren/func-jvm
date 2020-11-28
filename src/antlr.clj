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

(defn convert-constant [[first-elem :as s]]
  (case first-elem
    \" (trim-start-end s)
    \# (let [content (trim-start-end s)]
         (case (first content)
           \P (java.time.Duration/parse content)
           \- (java.time.Duration/parse content)
           (java.time.Instant/parse content)))
    :lang_float (BigDecimal. (second s))
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
    (= (ffirst t) :qualified_name)
    (into [(-> t first rest skip-odd)] (map convert-type (rest t)))

    :else
    (throw (ex-info "convert-type: unknown exp type" {:t t}))))

(def antlr-parse-csl-type (a/parser "csl.g4" {:root "type_eof"}))
(-> "java::lang::List java::lang::Number" antlr-parse-csl-type second
                                        ;rest butlast first convert-type
    convert-type
    )
;;(-> "A B" antlr-parse-csl-type rest butlast first convert-type)
(type '(:type (:qualified_upper "I") (:type (:qualified_upper "A"))))

(defn convert-pattern [[_pattern [kind & args :as input]]]
  (case kind
    :pattern_identifier
    (let [[var] args]
      (with-meta [:pattern-identifier var] (meta input)))

    (throw (ex-info "convert-pattern: unknown exp type" {}))))

(defn- convert-csl-exp [[_ [kind & args :as input] :as total]]
  (case kind
    :constant
    (let [[s] args
          value (convert-constant s)]
      (with-meta [kind value] (meta input)))

    :qualified_name
    (let [qname (skip-odd args)
          qname (if (= (count qname) 1) (first qname) qname)]
      (with-meta [:variable qname] (meta input)))

    :if_exp
    (let [[_if _lpar cond _rpar then _else else] args]
      (with-meta [:if (convert-csl-exp cond) (convert-csl-exp then) (convert-csl-exp else)] (meta input)))

    :expression
    (case (count total)
      3
      (let [[_ t1 t2] total]
        [:invoke-function (convert-csl-exp t1) (convert-csl-exp t2)])
      4
      (let [[_ t1 s t2] total]
        (case s
          ":>" [:upcast (convert-csl-exp t1) (convert-type t2)]
          "." [:field-access (convert-csl-exp t1) t2]
          ))
      )

    :lambda
    (let [[_backslash & cases] args]
      (when (< 1 (count cases)) (throw (ex-info "only one case supported for now" {:cases cases})))
      (let [case (first cases)
            [_ pat _arrow body] case
            cpat (convert-pattern pat)
            cbody (convert-csl-exp body)]
        (with-meta [:function cpat cbody] (meta input))))

    :tuple_or_paren
    (let [[_lpar & elements-separators] (butlast args)
          elements (skip-odd elements-separators)]
      (if (= (count elements) 1)
        (-> elements first convert-csl-exp)
        (with-meta (into [:tuple] (map convert-csl-exp elements)) (meta input))))

    (throw (ex-info "convert-csl-exp: unknown exp type" {:kind kind :args args :c (count input)}))
    ))

(defn convert-tld [[_ [kind & args :as input] :as total]]
  (case kind
    :val_decl
    (let [[_val pat _eq exp] args]
      (with-meta [:val-decl (convert-pattern pat) (convert-csl-exp exp)] (meta input)))))

(def antlr-parse-csl-exp (a/parser "csl.g4" {:root "expression_eof"}))
(defn parse-csl-exp [s] (-> s antlr-parse-csl-exp second convert-csl-exp))

(def top-level-parser (a/parser "csl.g4"))
(defn parse-top-level [s] (map convert-tld (-> s top-level-parser butlast rest)))
