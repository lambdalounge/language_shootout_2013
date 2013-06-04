!SLIDE bullets
# [Scala](http://www.scala-lang.org/)
## [Matt Wittmann](mailto:matt@mattwittmann.com)
* Lambda Lounge Languages Shoot-out 2013

!SLIDE bullets transition=scrollUp
# Rosalind Problems Solved

* [Counting DNA Nucleotides](http://rosalind.info/problems/dna/)
* [Rabbits and Recurrence Relations](http://rosalind.info/problems/fib/)
* [Mendel's First Law](http://rosalind.info/problems/iprb/)

!SLIDE bullets transition=scrollUp
# Scala Traits

* Scala traits are like Java interfaces but can mix in executable code (coming in Java 8).
* All solutions implemented this trait:

		package com.mattwittmann.lambdalounge.languageshootout2013
		trait RosalindSolution { def mkString(input: String): String }
* A child trait was defined for each problem.

!SLIDE bullets transition=scrollUp
# Mendel's First Law

* Wrote only one implementation of trait MendelsFirstLaw
* Simple math.

!SLIDE code transition=scrollUp

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


!SLIDE smbullets transition=scrollUp
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

!SLIDE code transition=scrollUp

	def asList(input: String) = {
	  val map = input./:(Map[Char, Int]()) {(map, i) => map + ((i, (map.getOrElse(i, 0) + 1)))}
	  List('A', 'C', 'G', 'T').map {map.getOrElse(_, 0)}
	}

!SLIDE code transition=scrollUp

	def asList(input: String) = {
	  val map = scala.collection.mutable.Map[Char, Int]()
	  input.foreach {c => map += ((c, map.getOrElse(c, 0) + 1))}
	  List('A', 'C', 'G', 'T').map {map.getOrElse(_, 0)}
	}

!SLIDE code transition=scrollUp

	def asList(input: String) = {
	  var a, c, g, t = 0
	  for (i <- 0 until input.length()) {
	    input(i) match {
	      case 'A' => a += 1
	      case 'C' => c += 1
	      case 'G' => g += 1
	      case 'T' => t += 1
	    }
	  }
	  List(a, c, g, t)
	}

!SLIDE code transition=scrollUp

	type DnaNucleotideCount = Tuple4[Int, Int, Int, Int]
	
	def asList(input: String) = {
	  def headMatch(head: Char, counted: DnaNucleotideCount) = {
	    val (a, c, g, t) = counted
	    head match {
	      case 'A' => (a + 1, c, g, t)
	      case 'C' => (a, c + 1, g, t)
	      case 'G' => (a, c, g + 1, t)
	      case 'T' => (a, c, g, t + 1)
	      case _ => throw new RuntimeException("Unexpected value: " + head)
	      }
	  }
	  @tailrec
	  def count(in: List[Char], counted: DnaNucleotideCount): DnaNucleotideCount = in match {
	    case Nil => counted
	    case head :: tail => count(tail, headMatch(head, counted))
	  }
	  val (a, c, g, t) = count(input.toList, (0, 0, 0, 0))
	  List(a, c, g, t)
	}

!SLIDE code transition=scrollUp

	class ParallelDnaNucleotideCounter(dnaNucleotideCounter: DnaNucleotideCounter, splitAt: Int) extends DnaNucleotideCounter {
	  def asList(input: String) = {
	    val substrings = (0 to input.length() by splitAt).par.map { i =>
	      val maxBoundary = i + splitAt
	      val end = if (maxBoundary < input.length()) maxBoundary else input.length()
	      input.substring(i, end)
	    }
	    substrings.map {dnaNucleotideCounter.asList(_)} reduce {(a, b) => (a, b).zipped map { _ + _ } toList}
	  }
	}

!SLIDE bullets transition=scrollUp
# Rabbits and Recurrence Relations
* ForwardRabbitRecurrence is a tail-recursive implementation
  starting from the first month and counting to the last.

!SLIDE code transition=scrollUp

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