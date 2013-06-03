<!SLIDE bullets>
# [Scala](http://www.scala-lang.org/)
## [Matt Wittmann](mailto:matt@mattwittmann.com)
* Lambda Lounge Languages Shoot-out 2013

<!SLIDE bullets transition=scrollUp>
# Rosalind Problems Solved

* [Counting DNA Nucleotides](http://rosalind.info/problems/dna/)
* [Rabbits and Recurrence Relations](http://rosalind.info/problems/fib/)
* [Mendel's First Law](http://rosalind.info/problems/iprb/)

<!SLIDE bullets transition=scrollUp>
# Scala Traits

* Scala traits are like Java interfaces but can mix in executable code (coming in Java 8).
* All solutions implemented this trait:

		package com.mattwittmann.lambdalounge.languageshootout2013
		trait RosalindSolution { def mkString(input: String): String }
* A child trait was defined for each problem.

<!SLIDE bullets transition=scrollUp>
# Mendel's First Law

* Wrote only one implementation of trait MendelsFirstLaw
* Simple math.

<!SLIDE code transition=scrollUp>

	object DefaultMendelsFirstLaw extends MendelsFirstLaw {
	  def probabilityDominant(dominant: Int, heterozygous: Int, recessive: Int): Double = {
	    val k = dominant.toDouble
	    val m = heterozygous.toDouble
	    val n = recessive.toDouble
	    val total = k + m + n
	    val numerator = (k *(k-1)) + (2 * k * m) + (2 * k * n) + (m * n) + (0.75 * m * (m-1))
	    val denominator = total * (total - 1)
	    if (total == 0)
	      0
	    else
	      numerator / denominator
	  }
	}


<!SLIDE smbullets transition=scrollUp>
# Counting DNA Nucleotides

* Experimented with several implementations.
* Benchmarked.
    * The classical imperative style was fastest.
    * The recursive implementation was only 3.5 times slower.
    * An implementation iterating over each char and storing counts in a mutable map was twice
      as slow as the recursive implementation.
    * An immutable map passed along a left fold was the slowest of all, about three times slower than
      the recursive implementation.
* A map-reduce implementation was provided for a parallel approach.

<!SLIDE bullets transition=scrollUp>
# Rabbits and Recurrence Relations
* ForwardRabbitRecurrence is a tail-recursive implementation
  starting from the first month and counting to the last.

<!SLIDE code transition=scrollUp>

	def countRabbitPairs(months: Int, litterSize: Int): Int = {
	  @tailrec
	  def count(currentMonth: Int, months: Int, litterSize: Int, lastYoungPairs: Int, lastMaturePairs: Int, youngPairs: Int, maturePairs: Int): Int = {
	      currentMonth match {
	        case x if x == months => lastYoungPairs + lastMaturePairs
	        case _ => count(currentMonth + 1, months, litterSize, youngPairs, maturePairs, maturePairs * litterSize, youngPairs + maturePairs)
	      }
	    }
	    if (litterSize == 0)
	      0
	    else {
	      months match {
	        case 0 => 0
	        case _ => count(0, months, litterSize, 0, 0, 1, 0)
	      }
	    }
	  }
	}