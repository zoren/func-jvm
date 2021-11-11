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
    :integer (Long/parseLong (second s)) ; todo give graceful error message
    (case s
      "true" Boolean/TRUE
      "false" Boolean/FALSE
      (throw (ex-info "unknown constant" {:s s})))))

(defn convert-qname [names-seps]
  (let [names (skip-odd names-seps)]
    (if (= (count names) 1) (first names) (into [] names))))

(defn convert-type [[kind & args]]
  (case kind
    ;; :function_type
    ;; (into [(-> t first rest skip-odd)] (map convert-type args))

    :type_var_apply
    (let [[t-name & t-args] args]
      (into [(apply vector (-> t-name rest skip-odd))] (map convert-type t-args)))

    :function_type
    (into [])

    :type_var
    [(apply vector (-> args first rest skip-odd))]
    :paren_type
    (let [[_ t _] args]
      (-> t convert-type))
    :type_atom
    (-> args first convert-type)

    (throw (ex-info "convert-type: unknown type kind" {:kind kind}))))

(defn convert-pattern [[kind & args :as input]]
  (->
   (case kind
     :wildcard
     [:wildcard]

     :pattern_identifier
     (let [[var] args]
       [:pattern-identifier var])

     :constant_pattern
     (let [[[_ s]] args
           value (convert-constant s)]
       [:constant value])

     :tuple_or_paren_pattern
     (let [[_lpar & elements-separators] (butlast args)
           elements (map convert-pattern (skip-odd elements-separators))]
       (if (= (count elements) 1)
         (first elements)
         (into [:tuple] elements)))

     :type_annotation_pattern
     (let [[pat _colon type] args]
       [:type-annotation (convert-pattern pat) (convert-type type)])

     (throw (ex-info "convert-pattern: unknown pattern kind" {:kind kind})))

   (with-meta (meta input))))

(defn- convert-exp [[kind & args :as input]]
  (case kind
    :constant_exp
    (let [[[_ s]] args
          value (convert-constant s)]
      (with-meta [:constant value] (meta input)))

    :var_or_const
    (let [qname (-> args first rest skip-odd)
          qname (if (= (count qname) 1) (first qname) qname)]
      (with-meta [:variable qname] (meta input)))

    :if_exp
    (let [[_if _lpar cond _rpar then _else else] args]
      (with-meta [:if (convert-exp cond) (convert-exp then) (convert-exp else)] (meta input)))

    :binary_expression
    (let [[_ t1 s t2] input]
      [:binary-operator (keyword s) (convert-exp t1) (convert-exp t2)])

    :apply
    (let [[_ t1 t2] input]
      [:invoke-function (convert-exp t1) (convert-exp t2)])

    :upcast_annotation_exp
    (let [[_ t1 _ t2] input]
      [:upcast (convert-exp t1) (convert-type t2)])

    :field_access_exp
    (let [[_ t1 _ field-name] input]
      [:field-access (convert-exp t1) field-name])

    :unary_minus
    (let [[_ e] args]
      [:unary-minus (convert-exp e)])

    :lambda
    (let [[_backslash & cases] args]
      (with-meta (into [:function]
                       (map (fn [[_ pat _arrow body]]
                              [(convert-pattern pat) (convert-exp body)]) (skip-odd cases)))
        (meta input)))

    :tuple_or_paren_exp
    (let [[_lpar & elements-separators] (butlast args)
          elements (skip-odd elements-separators)]
      (if (= (count elements) 1)
        (-> elements first convert-exp)
        (with-meta (into [:tuple] (map convert-exp elements)) (meta input))))

    :let
    (let [[_let [_ & val_decls] _in body] args]
      (with-meta [:let
                  (map (fn [[_ _ pat _ exp]] [(convert-pattern pat) (convert-exp exp)]) val_decls)
                  (convert-exp body)] (meta input)))

    (throw (ex-info "convert-exp: unknown exp type" {:kind kind :args args :c (count input)}))))

(defn convert-tld [[kind & args :as input]]
  (case kind
    :top_level_val_decl
    (let [[_val pat _eq exp] args]
      (with-meta [:val-decl (convert-pattern pat) (convert-exp exp)] (meta input)))

    :type_decl
    (let [[_type type-name [_ & type-params] [type-decl-kind & type-kind]] args]
      (into
       [:type-decl type-name type-params]
       (case type-decl-kind
         :union_type_kind
         (into [:union]
               (map (fn [[_ _pipe ctor-name & ctor-param-types]]
                      (into [ctor-name] (map convert-type ctor-param-types))
                      ) type-kind))))

      )

    (throw (ex-info "convert-tld: unknown type" {:kind kind :args args :c (count input)}))))

(defn make-parser [options] (a/parser "lang.g4" (merge {:use-alternates? true} options)))

(def parse-type-raw (make-parser {:root "type_eof"}))
(defn parse-type [s] (-> s parse-type-raw second convert-type))

(def parse-pattern-raw (make-parser {:root "pattern_eof"}))
(defn parse-pattern [s] (-> s parse-pattern-raw second convert-pattern))

(def parse-exp-raw (make-parser {:root "expression_eof"}))
(defn parse-exp [s] (-> s parse-exp-raw second convert-exp))

(def parse-top-level-raw (make-parser {:root "source_text"}))
(defn parse-top-level [s] (map convert-tld (-> s parse-top-level-raw butlast rest)))

