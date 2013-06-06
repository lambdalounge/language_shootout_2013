(ns dna.test-dna
  (:require [clojure.test :refer :all]
            [dna.dna :refer :all]
            [criterium.core :as crit]))

(deftest perf
  (println "Constructing big input data")
  (let [base "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
        s (apply concat (repeat 100000 base))
        v (vec s)]

    (System/gc)
    (println "testing sequential version")
    (crit/bench (dna-count v))

    (dotimes [g 3]
      (binding [*GRANULARITY* (* 4096 (apply * (repeat g 2)))]
        (println "\ntesting reducer version, granularity= " *GRANULARITY*)
        (crit/bench (dna-count-par v))))))
