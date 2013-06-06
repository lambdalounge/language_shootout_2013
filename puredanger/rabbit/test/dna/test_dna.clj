(ns dna.test-dna
  (:require [clojure.test :refer :all]
            [dna.dna :refer :all]))

(deftest perf
  (println "Constructing big input data")
  (let [base "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
        s (apply concat (repeat 100000 base))
        v (vec s)]

    (println "testing sequential version")
    (dotimes [n 5]
      (System/gc)
      (time (dna-count v)))
    
    (println "\ntesting reducer version")
    (dotimes [n 5]
      (System/gc)
      (time (dna-count-par v)))))
