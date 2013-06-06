package com.mattwittmann.lambdalounge.languageshootout2013

import org.scalatest.FunSuite

class UT_MendelsFirstLaw extends FunSuite {
  test("Zeroes") {
    assert(0.0 == MendelsFirstLaw(0, 0, 0))
  }
  test("All recessive") {
    assert(0.0 == MendelsFirstLaw(0, 0, 100))
  }
  test("All dominant") {
    assert(1.0 == MendelsFirstLaw(100, 0, 0))
  }
  test("All heterozygous") {
    assert(0.75 == MendelsFirstLaw(0, 100, 0))
  }
  test("Half dominant and half recessive") {
    assert(745.0/990.0 == MendelsFirstLaw(50, 0, 50))
  }
  test("Half heterozygous and half recessive") {
    assert(4337.5/9900.0 == MendelsFirstLaw(0, 50, 50))
  }
  test("Half dominant and half heterozygous") {
    assert(9287.5/9900.0 == MendelsFirstLaw(50, 50, 0))
  }
  test ("Twos") {
    assert(47.0 / 60.0 == MendelsFirstLaw(2, 2, 2))
  }
}