(ns evaluate)

(defn fn->function [f]
  (reify java.util.function.Function
    (apply [this a]
      (f a))))

(defn eval-annotated-pattern [[kind & args]]
  (case kind
    :pattern-identifier
    (let [[parameter] args]
      (fn [env argument] (assoc env parameter argument)))))

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

      :variable
      (-> args first env)

      :function
      (let [[parameter-pattern body] args
            updater (eval-annotated-pattern parameter-pattern)
            f (fn [argument] (eval-annotated-exp (updater env argument) body))]
        (fn->function f))

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
