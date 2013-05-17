package com.mattwittmann.lambdalounge.languageshootout2013

import org.scalatest.FunSuite

/**
 * @see [[http://rosalind.info/problems/dna/]]
 */
class UT_DnaNucleotideCounter extends FunSuite {
  test("Sample per http://rosalind.info/problems/dna/") {
    val implementations = List(DnaNucleotideCounterImmutableMapFold, DnaNucleotideCounterMutableMapForEach,
        DnaNucleotideCounterImperative, DnaNucleotideCounterRecursive, DnaNucleotideCounterRecursiveNoImplicit,
        new ParallelDnaNucleotideCounter(DnaNucleotideCounterImperative, 20))
    implementations.foreach {it => assert("20 12 17 21" == it.mkString("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"))}
  }
}