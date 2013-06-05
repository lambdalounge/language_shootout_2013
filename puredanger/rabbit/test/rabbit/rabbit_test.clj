(ns rabbit.rabbit-test
  (:use clojure.test
        rabbit.rabbit))

(def expected1to3
  [[0 1 1 2 3 5 8 13 21 34]
   [0 1 1 3 5 11 21 43 85 171]
   [0 1 1 4 7 19 40 97 217 508]])

(def expected1to3big
  [[0M 1M 1M 2M 3M 5M 8M 13M 21M 34M]
   [0M 1M 1M 3M 5M 11M 21M 43M 85M 171M]
   [0M 1M 1M 4M 7M 19M 40M 97M 217M 508M]])

(deftest fibk-test
  (let [results (for [f [fibk fibk-reduce fibk-iterate]]
                  (for [k (range 1 4)]
                    (fseq f 10 k)))]
    (is (apply = expected1to3 results))))

(deftest fibk-big-test
  (let [results (for [f [fibk-reduce-big fibk-iterate-big]]
                  (for [k (range 1 4)]
                    (fseq f 10 k)))]
    (is (apply = expected1to3big results))))
