package com.mattwittmann.lambdalounge.languageshootout2013

import org.scalatest.FunSuite

class UT_RabbitRecurrence extends FunSuite {
  type RabbitRecurrenceAssertion = Tuple2[String, String]

  def testRabbitRecurrence(assertions: List[RabbitRecurrenceAssertion], implementations: RabbitRecurrence*) {
    implementations foreach { i => assertions foreach {a => assert(a._1 == i.countRabbitPairs(a._2))} }
  }

  test("Invasion of the zeroes") {
    testRabbitRecurrence(List(("0", "0 0"), ("0", "1 0")),
        ForwardRabbitRecurrence, BackwardRabbitRecurrence)
  }
  test("Fibonacci sequence f(n) where n in [0, 12]") {
    val expectedValues = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144)
    val foldStartValue = (List[RabbitRecurrenceAssertion](), 0)
    val buildList = (a: (List[RabbitRecurrenceAssertion], Int), e: Int) => ((e.toString, s"${a._2} 1") :: a._1, a._2 + 1)
    testRabbitRecurrence(expectedValues./:(foldStartValue)(buildList)._1,
        ForwardRabbitRecurrence, BackwardRabbitRecurrence)
  }
  test("Sample per http://rosalind.info/problems/fib/") {
    testRabbitRecurrence(List(("19", "5 3")), ForwardRabbitRecurrence)
  }
  test("Other") {
    testRabbitRecurrence(List(("3", "3 2")), ForwardRabbitRecurrence)
  }
}