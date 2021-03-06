(ns unify)

(defn mk-type-var [level]
  {:link (atom nil)
   :level (atom level)})

(defn type-var? [{:keys [link level]}]
  (and (instance? clojure.lang.Atom link)
       (instance? clojure.lang.Atom level)))

(defn type? [t]
  (and (vector? t) (seq t)))

(defn link-type-var [tv t] (reset! (:link tv) t))

(defn normalize [t]
  (cond
    (type-var? t)
    (if-let [linked-type @(:link t)]
      (let [normed-linked-type (normalize linked-type)]
        (link-type-var t normed-linked-type)
        normed-linked-type)
      t)

    (type? t)
    (into [(first t)] (map normalize (rest t)))

    :else
    (throw (ex-info "normalize: unknown arg" {:t t}))))

(defn type-vars [t]
  (distinct
   (if (vector? t)
     (mapcat type-vars (rest t))
     [t])))

(defn map-type-vars [f t]
  ((fn m [t]
     (if (vector? t)
       (into [(first t)] (map m (rest t)))
       (f t)))
   t))

(defn get-unbound-type-vars [level t]
  (filter (fn [tv] (< level @(:level tv))) (type-vars t)))

(defn generalize [t level]
  (let [nt (normalize t)
        non-free-vars (get-unbound-type-vars level nt)]
    [non-free-vars nt]))

(defn specialize [level tvs t]
  (let [tenv (into {} (map (fn [tv] [tv (mk-type-var level)]) tvs))]
    (map-type-vars (fn [tv] (tenv tv tv)) t)))

(defn int->str [i]
  (if (< i 26)
    (str (char (+ (int \a) i)))
    (str \v (- i 25))))

(defn renumber [t]
  (let [m (into {} (map (fn [tv i] [tv (-> i int->str keyword)]) (type-vars t) (range)))]
    (map-type-vars m t)))

(defn occurs-check [tv t]
  (doseq [tv2 (type-vars t)]
    (when (= tv tv2) (throw (ex-info "occurs-check: fail" {:tv tv :tv2 tv2})))))

(defn link-type-var-to-type [tv t]
  ;; occurs check
  (occurs-check tv t)

  ;; level out
  (let [level @(:level tv)]
    (doseq [tv (type-vars t)]
      (swap! (:level tv) (partial min level))))

  (reset! (:link tv) t))

(defn unify [t1 t2]
  (let [[t1 t2] [(normalize t1) (normalize t2)]]
    (cond
      (and (type-var? t1) (type-var? t2))
      (when-not (= t1 t2)
        (if (< @(:level t1) @(:level t2))
          (link-type-var-to-type t1 t2)
          (link-type-var-to-type t2 t1)))

      (and (type-var? t1) (type? t2))
      (link-type-var-to-type t1 t2)

      (and (type? t1) (type-var? t2))
      (link-type-var-to-type t2 t1)

      (and (type? t1) (type? t2))
      (do
        (when-not (= (first t1) (first t2))
          (throw (ex-info "types differ" {::error :types-differ
                                          :t1 t1 :t2 t2})))
        (when-not (= (count t1) (count t2))
          (throw (ex-info "type arity mismatch" {::error :types-arity-mismatch
                                                 :t1 t1 :t2 t2})))
        (doseq [[ta1 ta2] (map vector (rest t1) (rest t2))]
          (unify ta1 ta2)))

      :else
      (throw (ex-info "unhandled case" {:t1 t1 :t2 t2})))))

(comment
  (do
    (def tv0 (mk-type-var 0))
    (def tv1 (mk-type-var 0))
    )
  (normalize tv0)
  (unify tv0 tv1)
  (unify tv0 [:int])
  (unify tv0 [:string])
  (unify [:int] tv1)
  (unify [:int] [:float])
  (unify [:map [:int]] [:map [:int] [:int]])
  (unify [:map [:int] [:int]] [:map [:int] [:int]])
  (unify [:map [:int] [:int]] [:map [:int] [:float]])
  (unify [java.util.Map tv0 tv0] [java.util.Map [java.lang.Long] tv1])
  )
