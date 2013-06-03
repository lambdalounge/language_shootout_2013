package com.mattwittmann.lambdalounge.languageshootout2013

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

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
    val map = input./:(Map[Char, Int]()) {(map, i) => map + ((i, (map.getOrElse(i, 0) + 1)))}
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
 *
 * The first argument should be the number of times to run each implementation, defaulting to 20.
 * The second argument should be the input, defaulting to
 * AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC.
 */
object DnaNucleotideCounterMain extends App {
  val times = if (args.length > 0) Integer.parseInt(args(0)) else 20
  val input = if (args.length > 1) args(1) else "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
  val implementations = List(DnaNucleotideCounterImmutableMapFold, DnaNucleotideCounterMutableMapForEach,
      DnaNucleotideCounterImperative, DnaNucleotideCounterRecursive,
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 10),
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 20),
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 70),
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 71),
      new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 72))
  for (implementation <- implementations) {
    val start = System.currentTimeMillis()
    for (i <- 0 to 20) {
      implementation.mkString(input)
    }
    val end = System.currentTimeMillis()
    println(s"Implementation ${implementation.getName} took ${end - start} ms to run $times times.")
  }
}