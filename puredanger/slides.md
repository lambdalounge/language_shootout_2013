!SLIDE 
# Some Clojure Stuff #

Alex Miller (@puredanger)

!SLIDE 
# Counting DNA made easy #

    @@@ clojure
    (defn dna-count [s]
      (vals (into (sorted-map) 
                  (frequencies s))))

!SLIDE
# Counting DNA made fast #


!SLIDE
# fibk rabbits (recursive) #

    @@@ clojure
    (defn fibk [n k]
      (if (< n 2) 
        n
        (+ (fibk (- n 1) k)
           (* k (fibk (- n 2) k)))))

!SLIDE
# fibk rabbits (iterative) #

    @@@ clojure
    (defn fibk-iterate [n k]
      (let [ifn (fn [[f2 f1]] 
                  [f1 (+ f1 (* k f2))])
            all (iterate ifn [0 1])]
      (nth (map first all) n)))

!SLIDE
# fibk rabbits (bignum) #
 
    @@@ clojure
    (defn fibk-iterate [n k]
      (let [ifn (fn [[f2 f1]] 
                  [f1 (+ f1 (* k f2))])
            all (iterate ifn [0M 1M])]
      (nth (map first all) n)))

