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

(defn convert-pattern [[_pattern [kind & args :as input] :as total]]
  (case kind
    :wildcard
    (with-meta [:wildcard] (meta input))

    :pattern_identifier
    (let [[var] args]
      (with-meta [:pattern-identifier var] (meta input)))

    :pattern_tuple_or_paren
    (let [[_lpar & elements-separators] (butlast args)
          elements (skip-odd elements-separators)]
      (if (= (count elements) 1)
        (-> elements first convert-pattern)
        (throw (ex-info "todo tuples" {}))
        #_        (with-meta (into [:tuple] (map convert-csl-exp elements)) (meta input))))

    :pattern
    (case (count total)
      4
      (let [[_ pat _colon type] total]
        [:type-annotation (convert-pattern pat) (convert-type type)])

      (throw (ex-info "convert-pattern: unknown pattern kind" {:total total})))

    :constant
    (let [[s] args
          value (convert-constant s)]
      (with-meta [kind value] (meta input)))

    (throw (ex-info "convert-pattern: unknown pattern kind" {:kind kind}))))

(defn- convert-csl-exp [[kind & args :as input]]
  (case kind
    :constant_exp
    (let [[[_ c]] args
          value (convert-constant c)]
      (with-meta [:constant value] (meta input)))

    :var_or_const
    (let [qname (-> args first rest skip-odd)
          qname (if (= (count qname) 1) (first qname) qname)]
      (with-meta [:variable qname] (meta input)))

    :if_exp
    (let [[_if _lpar cond _rpar then _else else] args]
      (with-meta [:if (convert-csl-exp cond) (convert-csl-exp then) (convert-csl-exp else)] (meta input)))

    :binary_expression
    (let [[_ t1 s t2] input]
      [:binary-operator (keyword s) (convert-csl-exp t1) (convert-csl-exp t2)])

    :apply
    (let [[_ t1 t2] input]
      [:invoke-function (convert-csl-exp t1) (convert-csl-exp t2)])

    :upcast_annotation_exp
    (let [[_ t1 _ t2] input]
      [:upcast (convert-csl-exp t1) (convert-type t2)])

    :field_access_exp
    (let [[_ t1 _ field-name] input]
      [:field-access (convert-csl-exp t1) field-name])

    :unary_minus
    (let [[_ e] args]
      [:unary-minus (convert-csl-exp e)])

    :lambda
    (let [[_backslash & cases] args]
      (with-meta (into [:function]
                       (map (fn [[_ pat _arrow body]]
                              [(convert-pattern pat) (convert-csl-exp body)]) (skip-odd cases)))
        (meta input)))

    :tuple_or_paren
    (let [[_lpar & elements-separators] (butlast args)
          elements (skip-odd elements-separators)]
      (if (= (count elements) 1)
        (-> elements first convert-csl-exp)
        (with-meta (into [:tuple] (map convert-csl-exp elements)) (meta input))))

    :let
    (let [[_let [_ & val_decls] _in body] args]
      (with-meta [:let
                  (map (fn [[_ _ pat _ exp]] [(convert-pattern pat) (convert-csl-exp exp)]) val_decls)
                  (convert-csl-exp body)] (meta input)))

    (throw (ex-info "convert-csl-exp: unknown exp type" {:kind kind :args args :c (count input)}))
    ))

(defn convert-tld [[_ [kind & args :as input]]]
  (case kind
    :val_decl
    (let [[_val pat _eq exp] args]
      (with-meta [:val-decl (convert-pattern pat) (convert-csl-exp exp)] (meta input)))))

(def antlr-parse-csl-exp (a/parser "csl.g4" {:root "expression_eof" :use-alternates? true}))
(defn parse-csl-exp [s] (-> s antlr-parse-csl-exp second convert-csl-exp))

(def top-level-parser (a/parser "csl.g4"))
(defn parse-top-level [s] (map convert-tld (-> s top-level-parser butlast rest)))
