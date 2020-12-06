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

  (testing "check unify does not overwrite links with types and order doesn't matter"
    (let [[f x a r] [(mk-type-var 0) (mk-type-var 0) (mk-type-var 0) (mk-type-var 0)]]
      (unify a f)
      (unify r [Long])
      (unify a x)
      (unify r x)

      (is (= [Long] (normalize f)))
      (is (= [Long] (normalize a))))
    (let [[f x a r] [(mk-type-var 0) (mk-type-var 0) (mk-type-var 0) (mk-type-var 0)]]
      (unify a f)
      (unify [Long] r)
      (unify a x)
      (unify r x)

      (is (= [Long] (normalize f)))
      (is (= [Long] (normalize a))))
    )
  )
