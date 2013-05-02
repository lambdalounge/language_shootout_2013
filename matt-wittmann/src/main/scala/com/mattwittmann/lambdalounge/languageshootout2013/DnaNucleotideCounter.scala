package com.mattwittmann.lambdalounge.languageshootout2013

import collection.mutable.Map

object DnaNucleotideCounter {
  def count(input: String) = {
    val map = Map[Char, Int]()
    input.foreach {c => map += ((c, map.getOrElse(c, 0) + 1))}
    List('A', 'C', 'G', 'T').seq.map {map.getOrElse(_, 0)}.mkString(" ")
  }
}