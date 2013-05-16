package com.mattwittmann.lambdalounge.languageshootout2013

import java.util.Date

/**
 * Trait defining the interface of a DNA nucleotide counter.
 */
trait DnaNucleotideCounter {
  def mkString(input: String): String = asList(input).mkString(" ")
  def asList(input: String): List[Int]
}

object DnaNucleotideCounterImmutableMapFold extends DnaNucleotideCounter {
  def asList(input: String) = {
    val map = input.foldLeft(Map[Char, Int]()) {(map, i) => map + ((i, (map.getOrElse(i, 0) + 1)))}
    List('A', 'C', 'G', 'T').map {map.getOrElse(_, 0)}
  }
}

object DnaNucleotideCounterMutableMapForEach extends DnaNucleotideCounter {
  import scala.collection.mutable.Map

  def asList(input: String) = {
    val map = Map[Char, Int]()
    input.foreach {c => map += ((c, map.getOrElse(c, 0) + 1))}
    List('A', 'C', 'G', 'T').map {map.getOrElse(_, 0)}
  }
}

object DnaNucleotideCounterImperative extends DnaNucleotideCounter {
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

object DnaNucleotideCounterRecursive extends DnaNucleotideCounter {
  type DnaNucleotideCount = Tuple4[Int, Int, Int, Int]

  implicit def dnaNucleotideCountWrapper(count: DnaNucleotideCount) = new {
    def ::(b: DnaNucleotideCount): DnaNucleotideCount = {
      val (aa, ac, ag, at) = count
      val (ba, bc, bg, bt) = b
      (aa + ba, ac + bc, ag + bg, at + bt)
    }
  }

  def asList(input: String) = {
    def count(in: Any, counted: DnaNucleotideCount): DnaNucleotideCount = in match {
      case "" => counted
      case tail: String => count(tail.head, counted) :: count(tail.tail, counted)
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

object DnaNucleotideCounterRecursiveNoImplicit extends DnaNucleotideCounter {
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

object DnaNucleotideCounterMain extends App {
  val implementations = List(DnaNucleotideCounterImmutableMapFold, DnaNucleotideCounterMutableMapForEach, DnaNucleotideCounterImperative, DnaNucleotideCounterRecursive, DnaNucleotideCounterRecursiveNoImplicit)
  val input = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
  for (implementation <- implementations) {
    val start = System.currentTimeMillis()
    for (i <- 0 to 20) {
      implementation.mkString(input)
    }
    val end = System.currentTimeMillis()
    println(s"Implementation ${implementation.getClass().getName()} took ${end - start} milliseconds to run twenty times.")
  }
}