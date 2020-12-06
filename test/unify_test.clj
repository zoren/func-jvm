(ns unify-test
  (:require [unify :refer [mk-type-var unify normalize]]
            [clojure.test :refer [deftest is testing]]
            ))

(deftest unify-test
  (testing "check unify does not overwrite links and order doesn't matter"
    (let [[x a r] [(mk-type-var 0) (mk-type-var 0) (mk-type-var 0)]]
      (unify a x)
      (unify r x)
      (is (apply = (map normalize [x a r]))))
    (let [[x a r] [(mk-type-var 0) (mk-type-var 0) (mk-type-var 0)]]
      (unify x a)
      (unify x r)
      (is (apply = (map normalize [x a r]))))
    )
  )
