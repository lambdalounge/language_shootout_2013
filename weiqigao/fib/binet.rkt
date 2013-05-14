#lang racket

(require racket)
(require math/bigfloat)


(define (fib n k)
  (letrec ([root (bfsqrt (bf+ (bf 1/4) k))]
           [alpha (bf- (bf 1/2) root)]
           [beta (bf+ (bf 1/2) root)])
    (bigfloat->integer (bffloor (bf+ (bf/ (bf- (bfexpt beta n) (bfexpt alpha n)) root (bf 2)) (bf 1/2))))))

(bf-precision 80)

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
          (display (fib (bf n) (bf k)))
          (newline))))
)
