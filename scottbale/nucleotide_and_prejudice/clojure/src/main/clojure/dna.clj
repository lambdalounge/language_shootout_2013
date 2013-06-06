(ns dna)

(defn count-nucleotides [word]
  (reduce (fn [counts symb]
            (let [[k count] (find counts symb)]
              (apply assoc counts [k (inc count)])))
          {\A 0 \C 0 \G 0 \T 0}
          word))

(defn formatted-count [counts]
  (apply str (interpose " " (map counts [\A \C \G \T]))))

(defn formatted-count [counts]
  (apply format "%d %d %d %d" (map counts [\A \C \G \T])))

(comment
  
  (formatted-count (count-nucleotides "ACGCATGAAT"))

  )