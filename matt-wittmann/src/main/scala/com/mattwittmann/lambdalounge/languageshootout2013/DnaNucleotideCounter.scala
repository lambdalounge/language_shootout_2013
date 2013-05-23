package com.mattwittmann.lambdalounge.languageshootout2013

import scala.collection.mutable.ArrayBuffer

/**
 * Trait defining the interface of a DNA nucleotide counter.
 */
trait DnaNucleotideCounter extends RosalindSolution {
  /**
   * Returns a String with the count of DNA nucleotides (A, C, G, T).
   *
   * @param input A string of nucleotides
   * @return The count of each nucleotide: "A C G T"
   */
  def mkString(input: String): String = asList(input).mkString(" ")
  /**
   * Returns a list of DNA nucleotides.
   *
   * @param input A string of nucleotides
   * @return The count of each nucleotide: List(A, C, G, T)
   */
  def asList(input: String): List[Int]

  /**
   * Gets the name of the implementation.
   *
   * @return A user-friendly name for the implementation object
   */
  def getName(): String
}

/**
 * This implementation of [[com.mattwittmann.lambdalounge.languageshootout2013.DnaNucleotideCounter]]
 * is purely functional: immutable collection types, final values, and higher-order functions.
 */
object DnaNucleotideCounterImmutableMapFold extends DnaNucleotideCounter {
  def getName() = "Immutable Map Counter"

  def asList(input: String) = {
    val map = input.foldLeft(Map[Char, Int]()) {(map, i) => map + ((i, (map.getOrElse(i, 0) + 1)))}
    List('A', 'C', 'G', 'T').map {map.getOrElse(_, 0)}
  }
}

/**
 * This implementation of [[com.mattwittmann.lambdalounge.languageshootout2013.DnaNucleotideCounter]]
 * is a compromise of the functional paradigm with mutable collection types.
 */
object DnaNucleotideCounterMutableMapForEach extends DnaNucleotideCounter {
  import scala.collection.mutable.Map

  def getName() = "Mutable Map Counter"

  def asList(input: String) = {
    val map = Map[Char, Int]()
    input.foreach {c => map += ((c, map.getOrElse(c, 0) + 1))}
    List('A', 'C', 'G', 'T').map {map.getOrElse(_, 0)}
  }
}

/**
 * This implementation of [[com.mattwittmann.lambdalounge.languageshootout2013.DnaNucleotideCounter]]
 * is in the classic imperative style familiar to programmers using Java, C, Pascal, etc.
 */
object DnaNucleotideCounterImperative extends DnaNucleotideCounter {
  def getName() = "Imperative Counter"

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
}

/**
 * This implementation of [[com.mattwittmann.lambdalounge.languageshootout2013.DnaNucleotideCounter]]
 * uses a functional style featuring recursion.
 */
object DnaNucleotideCounterRecursive extends DnaNucleotideCounter {
  def getName() = "Recursive Counter"

  type DnaNucleotideCount = Tuple4[Int, Int, Int, Int]

  /**
   * Provides a little syntactic sugar for dealing with this tuple DnaNucleotideCount.
   *
   * @param count The tuple of DNA nucleotides counted so far
   * @return A wrapper object of the type DnaNucleotideCount
   */
  implicit def dnaNucleotideCountWrapper(count: DnaNucleotideCount) = new {
    /**
     * Adds two DnaNucleotideCount tuples.
     *
     * @param b The DnaNucleotideCount tuple to be added to count
     * @return A new DnaNucleotideCount the elements of the same arity added 
     */
    def +(b: DnaNucleotideCount): DnaNucleotideCount = {
      val (aa, ac, ag, at) = count
      val (ba, bc, bg, bt) = b
      (aa + ba, ac + bc, ag + bg, at + bt)
    }
  }

  def asList(input: String) = {
    def count(in: Any, counted: DnaNucleotideCount): DnaNucleotideCount = in match {
      case "" => counted
      case tail: String => count(tail.head, counted) + count(tail.tail, counted)
      case head: Char => {
        val (a, c, g, t) = counted
        head match {
          case 'A' => (a + 1, c, g, t)
          case 'C' => (a, c + 1, g, t)
          case 'G' => (a, c, g + 1, t)
          case 'T' => (a, c, g, t + 1)
          case _ => throw new RuntimeException("Unexpected value: " + head)
        }
      }
      case _ => throw new RuntimeException("Unexpected value: " + in)
    }
    val (a, c, g, t) = count(input, (0, 0, 0, 0))
    List(a, c, g, t)
  }
}

/**
 * This implementation of [[com.mattwittmann.lambdalounge.languageshootout2013.DnaNucleotideCounter]]
 * uses a functional style featuring recursion.
 *
 * Unlike [[com.mattwittmann.lambdalounge.languageshootout2013.DnaNucleotideCounterRecursive]],
 * this implementation forgoes the implicit type conversion that adds some overhead.
 */
object DnaNucleotideCounterRecursiveNoImplicit extends DnaNucleotideCounter {
  def getName() = "Recursive Counter Without Implicit Conversion"

  type DnaNucleotideCount = Tuple4[Int, Int, Int, Int]

  def asList(input: String) = {
    def count(in: Any, counted: DnaNucleotideCount): DnaNucleotideCount = in match {
      case "" => counted
      case tail: String => {
        val (aa, ac, ag, at) = count(tail.head, counted)
        val (ba, bc, bg, bt) = count(tail.tail, counted)
        (aa + ba, ac + bc, ag + bg, at + bt)
      }
      case head: Char => {
        val (a, c, g, t) = counted
        head match {
          case 'A' => (a + 1, c, g, t)
          case 'C' => (a, c + 1, g, t)
          case 'G' => (a, c, g + 1, t)
          case 'T' => (a, c, g, t + 1)
          case _ => throw new RuntimeException("Unexpected value: " + head)
        }
      }
      case _ => throw new RuntimeException("Unexpected value: " + in)
    }
    val (a, c, g, t) = count(input, (0, 0, 0, 0))
    List(a, c, g, t)
  }
}

/**
 * This implementation enables the string to be broken up with each calculated in parallel
 * using the popular map-reduce pattern.
 *
 * @param dnaNucleotideCounter A DnaNucleotideCounter whose implementation should be used for the calculations
 * @param splitAt The number of characters to split the string at
 */
class ParallelDnaNucleotideCounter(dnaNucleotideCounter: DnaNucleotideCounter, splitAt: Int) extends DnaNucleotideCounter {
  def getName() = s"Parallel ($splitAt) ${dnaNucleotideCounter.getName}"

  def asList(input: String) = {
    val substrings = new ArrayBuffer[String]
    for (i <- 0 to input.length() by splitAt) {
      val maxBoundary = i + splitAt
      val end = if (maxBoundary < input.length()) maxBoundary else input.length()
      substrings += input.substring(i, end)
    }
    // TODO Using par slows down the calculations considerably at least for small data sets
    val result = substrings.toList.map {dnaNucleotideCounter.asList(_)} reduce {(a, b) =>
      val reduced = new ArrayBuffer[Int]
      for (j <- 0 until a.length) {
        val sum = if (j < b.length) a(j) + b(j) else a(j)
        reduced += sum
      }
      reduced.toList
    }
    result
  }
}

/**
 * Entry point of the application.
 */
object DnaNucleotideCounterMain extends App {
  val times = 20
  val implementations = List(DnaNucleotideCounterImmutableMapFold, DnaNucleotideCounterMutableMapForEach,
      DnaNucleotideCounterImperative, DnaNucleotideCounterRecursive, DnaNucleotideCounterRecursiveNoImplicit,
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 10),
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 20),
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 70),
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 71),
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 72))
  val input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
  for (implementation <- implementations) {
    val start = System.currentTimeMillis()
    for (i <- 0 to 20) {
      implementation.mkString(input)
    }
    val end = System.currentTimeMillis()
    println(s"Implementation ${implementation.getName} took ${end - start} ms to run $times times.")
  }
}