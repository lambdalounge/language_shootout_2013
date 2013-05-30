(ns dna)

(defn count-nucleotides [word]
  (let [counts {\A 0
                \C 0
                \G 0
                \T 0}]
    (reduce (fn [counts symb]
              (let [[k count] (find counts symb)]
                (apply assoc counts [k (inc count)])))
            counts
            word)))

(defn formatted-count [counts]
  (apply str (interpose " " (map (comp str counts) [\A \C \G \T]))))

(comment
  
  (formatted-count (count-nucleotides "ACGCATGAAT"))


  )