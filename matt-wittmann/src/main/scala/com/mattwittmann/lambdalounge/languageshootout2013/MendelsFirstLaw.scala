package com.mattwittmann.lambdalounge.languageshootout2013

/**
 * Finds the probability for the dominant phenotype in a population with
 * one dominant and one recessive allele of a gene, so the three genotypes would
 * be DD, DR, and RR.
 *
 * @see [[http://rosalind.info/problems/iprb/]]
 */
trait MendelsFirstLaw extends RosalindSolution {
  def mkString(input: String): String = {
    val split = input split " "
    if (split.length == 3) {
      probabilityDominant(split(0).toInt, split(1).toInt, split(2).toInt) toString
    }
    else
      ""
  }

  /**
   * Calculate the probability a couple from a population with these genotypes would produce offspring
   * with the dominant phenotype.
   *
   * @param dominant The number of organisms with the DD genotype
   * @param heterozygous The number of organisms with the DR genotype
   * @param recessive The number of organisms with the RR genotype
   * @return The probability a random pairing from this population will produce an offspring
   * of the the dominant phenotype
   */
  def probabilityDominant(dominant: Int, heterozygous: Int, recessive: Int): Double
}

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

object MendelsFirstLaw {
  def apply(input: String): String = DefaultMendelsFirstLaw.mkString(input)
  def apply(dominant: Int, heterozygous: Int, recessive: Int): Double = DefaultMendelsFirstLaw.probabilityDominant(dominant, heterozygous, recessive)
}

object MendelsFirstLawMain extends App {
  println(MendelsFirstLaw("2 2 2"))
}