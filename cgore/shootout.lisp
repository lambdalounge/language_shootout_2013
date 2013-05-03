(require :asdf)
(require :sigma)
(sigma:use-all-sigma)

;;; Counting DNA Nucleotides
;;; http://rosalind.info/problems/dna/
;;;
;;; A string is simply an ordered collection of symbols selected from some
;;; alphabet and formed into a word; the length of a string is the number of
;;; symbols that it contains.
;;;
;;; An example of a length 21 DNA string (whose alphabet contains the symbols
;;; 'A', 'C', 'G', and 'T') is "ATGCTTCAGAAAGGTCTTACG."
;;;
;;; Given: A DNA string s of length at most 1000 nt.
;;;
;;; Return: Four integers (separated by spaces) counting the respective number
;;; of times that the symbols 'A', 'C', 'G', and 'T' occur in s.
;;;
;;; Sample Dataset
;;;
;;; AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
;;;
;;; Sample Output
;;;
;;; 20 12 17 21

(defun count-dna (input)
  (assert (stringp input))
  (let ((counts (make-hash-table)))
    (map 'list (rcurry 'inchash counts) input)
    (string-join (mapcar 'stringify
			 (mapcar (rcurry 'gethash counts)
				 (map 'list 'identity "ACGT")))
		 " ")))

(assert
 (equalp
  (count-dna "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
  "20 12 17 21"))

;;; Rabbits and Recurrence Relations
;;; http://rosalind.info/problems/fib/
;;;
;;; A sequence is an ordered collection of objects (usually numbers), which are
;;; allowed to repeat. Sequences can be finite or infinite. Two examples are the
;;; finite sequence (π,−√2,0,π) and the infinite sequence of odd numbers
;;; (1,3,5,7,9,…). We use the notation an to represent the n-th term of a
;;; sequence.
;;;
;;; A recurrence relation is a way of defining the terms of a sequence with
;;; respect to the values of previous terms. In the case of Fibonacci's rabbits
;;; from the introduction, any given month will contain the rabbits that were
;;; alive the previous month, plus any new offspring. A key observation is that
;;; the number of offspring in any month is equal to the number of rabbits that
;;; were alive two months prior. As a result, if Fn represents the number of
;;; rabbit pairs alive after the n-th month, then we obtain the Fibonacci
;;; sequence having terms Fn that are defined by the recurrence relation
;;; Fn=Fn−1+Fn−2 (with F1=F2=1 to initiate the sequence). Although the sequence
;;; bears Fibonacci's name, it was known to Indian mathematicians over two
;;; millennia ago.
;;;
;;; When finding the n-th term of a sequence defined by a recurrence relation,
;;; we can simply use the recurrence relation to generate terms for
;;; progressively larger values of n. This problem introduces us to the
;;; computational technique of dynamic programming, which successively builds up
;;; solutions by using the answers to smaller cases.
;;;
;;; Given: Positive integers n≤40 and k≤5.
;;;
;;; Return: The total number of rabbit pairs that will be present after n months
;;; if each pair of reproduction-age rabbits produces a litter of k rabbit pairs
;;; in each generation (instead of only 1 pair).  Sample Dataset
;;;
;;; 5 3
;;;
;;; Sample Output
;;;
;;; 19

(defun bunnies (n k)
  (if (<= n 2)
      1
      (+ (bunnies (- n 1) k)
	 (* k (bunnies (- n 2) k)))))

(assert (= (bunnies 5 3) 19))
