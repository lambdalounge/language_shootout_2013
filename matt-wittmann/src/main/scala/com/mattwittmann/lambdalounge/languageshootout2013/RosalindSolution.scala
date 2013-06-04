package com.mattwittmann.lambdalounge.languageshootout2013

/**
 * A generalized trait for a solution to one of the bioinformatics problems on the Rosalind website.
 *
 * @see [[http://rosalind.info/]]
 */
trait RosalindSolution {
  /**
   * Builds a string representation of the solution from string input.
   *
   * @param input The problem input
   * @return The solution to the problem for the given input
   */
  def mkString(input: String): String
}