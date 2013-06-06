(ns dna.dna
  (:require [clojure.core.reducers :as r]))

;; easy

(defn dna-count [s]
  (vals (into (sorted-map) (frequencies s))))


;; now with reducers

(defn dna-count-par [s]
  (let [v (vec s)
        seed {\C 0 \G 0 \T 0 \A 0}
        combinef (fn [& x] (if x (apply merge-with + x) seed))
        reducef (fn [counts x] (update-in counts [x] inc))
        totals (r/fold combinef reducef v)]
    (vals (into (sorted-map) totals))))

(defn dna-count-par2 [s]
  (let [seed {\C 0 \G 0 \T 0 \A 0}
        combinef (fn [& x] (if x (apply merge-with + x) seed))
        reducef (fn [counts x]
                  (persistent!
                   (assoc! (transient counts) x (inc (get counts x)))))
        totals (r/fold 65536 combinef reducef s)]
    (vals (into (sorted-map) totals))))

