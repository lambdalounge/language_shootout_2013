package com.mattwittmann.lambdalounge.languageshootout2013

/**
 * Rabbits and Recurrence Relations is a problem of Fibonacci sequences.
 *
 * @see [[http://rosalind.info/problems/fib/]]
 */
trait RabbitRecurrence extends RosalindSolution {
  /**
   * Counts the number of rabbits.
   *
   * @param input Of the form "months litterSize"
   * @return The total number of rabbits
   */
  def mkString(input: String): String = {
    val split = input split " "
    if (split.length == 2)
      countRabbitPairs(split(0).toInt, split(1).toInt) toString
    else
      ""
  }

  /**
   * Counts the number of rabbits.
   *
   * @param months Referred to as ''n'' in the problem description, is the number of months past
   * @param litterSize Referred to as ''k'' in the problem description, is the number of rabbit pairs per
   *        litter for each generation
   * @return The total number of rabbits
   */
  def countRabbitPairs(months: Int, litterSize: Int): Int
}

/**
 * Implements the RabbitRecurrence traits using a basic recursive algorithm.
 */
object ForwardRabbitRecurrence extends RabbitRecurrence {
  def countRabbitPairs(months: Int, litterSize: Int): Int = {
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

/**
 * Implements the RabbitRecurrence trait using a recursive algorithm, going down from the last month.
 *
 * FIXME This does not work for months > 1.
 */
object BackwardRabbitRecurrence extends RabbitRecurrence {
  def countRabbitPairs(months: Int, litterSize: Int): Int = {
    if (litterSize == 0)
      0
    else
      months match {
      case 0 => 0
      case 1 => 1
      case _ => countRabbitPairs(months - 2, litterSize) + countRabbitPairs(months - 1, litterSize)
      }
  }
}