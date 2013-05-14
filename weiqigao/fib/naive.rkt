#lang racket

(require racket)

(define (fib n k)
  (case n
    [(1 2) 1]
    [else (+ (fib (- n 1) k) (* k (fib (- n 2) k)))]))
    

(let ([argv (current-command-line-arguments)])
  (if (or (not (= (vector-length argv) 2))
          (let ([n (string->number (vector-ref argv 0))]
                [k (string->number (vector-ref argv 1))])
            (or (< n 1)
                (> n 40)
                (< k 1)
                (> k 5))))
      (begin (display "usage: racket naive.rkt <n> <k> where 1 <= n <= 40 and 1 <= k <= 5")
             (newline)
             (exit))
      (let ([n (string->number (vector-ref argv 0))]
            [k (string->number (vector-ref argv 1))])
        (begin
          (display (fib n k))
          (newline))))
)
