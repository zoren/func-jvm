(ns evaluate)

(defn fn->function [f]
  (reify java.util.function.Function
    (apply [this a]
      (f a))))

(defn eval-annotated-pattern [[kind & args]]
  (case kind
    :wildcard
    (fn [env _argument] env)

    :pattern-identifier
    (let [[parameter] args]
      (fn [env argument] (assoc env parameter argument)))

    :constant
    (let [[c] args]
      (fn [env argument] (when (= c argument) env)))))

(defn eval-annotated-exp [env [kind & args :as exp]]
  (let [eval-args (fn [args] (into-array Object (map (partial eval-annotated-exp env) args)))]
    (case kind
      :constant
      (first args)

      :class
      (-> exp meta :class)

      :get-static-field
      (let [class-obj (-> exp meta :class)
            field (-> exp meta :field)]
        (.get field class-obj))

      :get-instance-field
      (let [[instance-exp _field-name] args
            instance (eval-annotated-exp env instance-exp)
            field (-> exp meta :field)]
        (.get field instance))

      :construct
      (let [ctor (-> exp meta :ctor)
            [_ & args] args]
        (.newInstance ctor (eval-args args)))

      :invoke-static-method
      (let [[_class-name _method-name & args] args
            method (-> exp meta :method)]
        (.invoke method nil (eval-args args)))

      :invoke-instance-method
      (let [[instance-exp _method-name & args] args
            instance (eval-annotated-exp env instance-exp)
            method (-> exp meta :method)]
        (.invoke method instance (eval-args args)))

      :if
      (let [[cond t f] args]
        (if (eval-annotated-exp env cond)
          (eval-annotated-exp env t)
          (eval-annotated-exp env f)))

      :unary-minus
      (let [[e] args]
        (- (eval-annotated-exp env e)))

      :binary-operator
      (let [[operator e1 e2] args]
        (cond
          (#{:+ :- :*
             :<= :>= :< :> :=} operator)
          ;; TODO make sure we handle wrapping of integers correctly
          ;; use math context 128 for bigdecimals
          ;; comparison of instants
          ;; allow comparison operators for all comparables
          ((-> operator name symbol eval) (eval-annotated-exp env e1) (eval-annotated-exp env e2))

          (= :/ operator)
          (let [arg-type (-> e1 meta :type first)]
            (cond
              (= arg-type BigDecimal)
              (binding [*math-context* java.math.MathContext/DECIMAL128]
                (/ (eval-annotated-exp env e1) (eval-annotated-exp env e2)))
              (= arg-type Long)
              (quot (eval-annotated-exp env e1) (eval-annotated-exp env e2))))

          (= :&& operator)
          (if (eval-annotated-exp env e1)
            (eval-annotated-exp env e2)
            false)

          (= :|| operator)
          (if (eval-annotated-exp env e1)
            true
            (eval-annotated-exp env e2))
          )
        )

      :variable
      (-> args first env)

      :function
      (fn->function
       (fn [argument]
         (loop [cases args]
           (if (empty? cases)
             (throw (ex-info "pattern match not exhaustive" {:cases args :argument argument}))
             (let [[[parameter-pattern body] & cases] cases]
               (if-let [env1 ((eval-annotated-pattern parameter-pattern) env argument)]
                 (eval-annotated-exp env1 body)
                 (recur cases)))))))

      :let
      (let [[val-decls body] args
            env3 (reduce (fn [env [pat exp]]
                           (let [updater (eval-annotated-pattern pat)
                                 v (eval-annotated-exp env exp)
                                 env1 (updater env v)]
                             (when-not env1 (throw (ex-info "pattern did not match" {:pat pat})))
                             env1
                             )) env val-decls)]
        (eval-annotated-exp env3 body))

      :invoke-function
      (let [[func arg] args]
        (.apply (eval-annotated-exp env func) (eval-annotated-exp env arg)))

      (throw (ex-info "eval-exp: unknown exp type" {:exp exp})))))

(defn eval-decl [env [kind & args]]
  (case kind
    :val-decl
    (let [[pattern expression] args
          updater (eval-annotated-pattern pattern)
          a-exp (eval-annotated-exp env expression)]
      (updater env a-exp))

    (throw (ex-info "eval-decl: unknown decl type" {:kind kind :args args}))))

(defn eval-decls [env decls] (reduce eval-decl env decls))
