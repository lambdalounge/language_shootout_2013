<!SLIDE title-slide>

# [Racket](http://racket-lang.org/)
## [Weiqi Gao](http://weiqigao.blogspot.com/)
## [@weiqigao](http://twitter.com/weiqigao)
## [Object Computing, Inc.](http://www.ociweb.com)

<!SLIDE incremental transition=scrollUp>

# I will:

  * Solve the "[Rabbits & Recurrent Relations](http://rosalind.info/problems/fib/)" problem
  * In [Racket](http://racket-lang.org/), a functional language in the Scheme tradition
  * In three ways
    * [The naive](https://github.com/lambdalounge/language_shootout_2013/blob/master/weiqigao/fib/naive.rkt) (exponantial time)
    * [The iterative](https://github.com/lambdalounge/language_shootout_2013/blob/master/weiqigao/fib/iterative.rkt) (linear time)
    * [The smart(?)](https://github.com/lambdalounge/language_shootout_2013/blob/master/weiqigao/fib/binet.rkt) (constant time)

<!SLIDE incremental transition=scrollUp>

# Rabbits & Recurrent Relations

  * F_1 = 1
  * F_2 = 1
  * F\_n = F\_(n-1) + k F_(n-2), n = 3, 4, 5, ...

  * Find F_n for n <=40 and k <= 5.

<!SLIDE code transition=scrollUp>
.notes naive.rkt

	(require racket)

	(define (fib n k)
	  (case n
	    [(1 2) 1]
	    [else (+ (fib (- n 1) k)
	             (* k (fib (- n 2) k)))]))

<!SLIDE code transition=scrollUp>
.notes iterative.rkt

	(require racket)

	(define (fib-iter a b n k)
	  (case n
	    [(1) b]
	    [else (fib-iter (+ a (* k b)) a
	                    (- n 1) k)]))

	(define (fib n k)
	  (fib-iter 1 1 n k))

<!SLIDE code transition=scrollUp>
.notes binet.rkt

	(require racket)
	(require math/bigfloat)

	(define (fib n k)
	  (letrec ([root (bfsqrt (bf+ (bf 1/4) k))]
	           [alpha (bf- (bf 1/2) root)]
	           [beta (bf+ (bf 1/2) root)])
	    (bigfloat->integer
	     (bffloor
	      (bf+
	       (bf/
	        (bf-
	         (bfexpt beta n)
	         (bfexpt alpha n))
	        root (bf 2)) (bf 1/2))))))

	(bf-precision 80)

<!SLIDE commandline incremental transition=scrollUp>

# Execution times

	$ for fib in naive.rkt iterative.rkt binet.rkt; \
	do for n in 37 38 39 40; do time racket $fib $n 5 ; \
	done; done 2>&1 | grep real
	real    0m1.037s
	real    0m1.324s
	real    0m1.796s
	real    0m2.537s
	real    0m0.557s
	real    0m0.550s
	real    0m0.560s
	real    0m0.556s
	real    0m1.017s
	real    0m1.017s
	real    0m1.023s
	real    0m1.022s
