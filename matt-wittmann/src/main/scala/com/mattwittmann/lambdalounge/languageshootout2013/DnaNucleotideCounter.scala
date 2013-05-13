package com.mattwittmann.lambdalounge.languageshootout2013

object DnaNucleotideCounter {
  def count(input: String) = {
    val map =input.foldLeft(Map[Char, Int]()) {(map, i) => map + ((i, (map.getOrElse(i, 0) + 1)))}
    List('A', 'C', 'G', 'T').seq.map {map.getOrElse(_, 0)}.mkString(" ")
  }
}