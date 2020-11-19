(ns antlr
  (:require
   [clj-antlr.core :as a]))

(def csl (a/parser "csl.g4"))
(clojure.pprint/pprint ((a/parser "csl.g4") "
val sameDay = 2
// eer
val fromJust = 3"))
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
