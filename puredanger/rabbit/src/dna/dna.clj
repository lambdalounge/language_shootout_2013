(ns dna.dna)

(defn dna-count [s]
  (vals (into (sorted-map) (frequencies s))))
