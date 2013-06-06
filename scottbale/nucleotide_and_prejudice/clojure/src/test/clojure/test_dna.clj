(ns test-dna
  (:use [clojure.test]
        [dna]))

(deftest test-count-nucleowhatchamadoodles []
  (let [input "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
        expected "20 12 17 21"]
    (is (= expected (formatted-count (count-nucleotides input))))))