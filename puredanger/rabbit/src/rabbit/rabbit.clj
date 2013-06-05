(ns rabbit.rabbit)

;; recursive
(defn fibk [n k]
  (if (< n 2) 
    n
    (+ (fibk (- n 1) k)
       (* k (fibk (- n 2) k)))))

;; using reduce 

(defn- fibk-reduce' [n k init]
  (if (zero? n)
    init
    (second
     (reduce (fn [[f2 f1] _] [f1 (+ f1 (* k f2))])
             [init (inc init)]
             (range (dec n))))))

(defn fibk-reduce [n k]
  (fibk-reduce' n k 0))

(defn fibk-reduce-big [n k]
  (fibk-reduce' n k 0M))

;; using iterate to remove bogus range and _

(defn fibk-iterate' [n k init]
  (let [all (iterate (fn [[f2 f1]] [f1 (+ f1 (* k f2))])
                     [init (inc init)])]
    (nth (map first all) n)))

(defn fibk-iterate [n k]
  (fibk-iterate' n k 0))

(defn fibk-iterate-big [n k]
  (fibk-iterate' n k 0M))

;; helper

(defn fseq [f n k]
  (map #(f % k) (range n)))
