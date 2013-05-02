package com.mattwittmann.lambdalounge.languageshootout2013

import org.scalatest.FunSuite

class UT_DnaNucleotideCounter extends FunSuite {
  test("Sample per http://rosalind.info/problems/dna/") {
    assert("20 12 17 21" == DnaNucleotideCounter.count("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"))
  }
}