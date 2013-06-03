package com.mattwittmann.lambdalounge.languageshootout2013

import scala.reflect.runtime.universe._
import scala.io.Source
import scala.collection.mutable.Map
import org.scalatest.FunSuite

/**
 * Runs test cases found in data.csv.
 *
 * The format of data.csv is:
 * Class,Input,Expected
 */
class TestHarness extends FunSuite {
  /** The number of times to run each test case. */
  val times = 200

  /**
   * Gets an instance of a Scala class/singleton object by its fully qualified name. Scala reflection
   * is used to discover a singleton object's instance or to instantiate a class using its constructor.
   *
   * @param fullName The fully qualified Java class name
   * @return An instance of [[com.mattwittmann.lambdalounge.languageshootout2013.RosalindSolution]]
   */
  def getInstanceByReflection(fullName: String): RosalindSolution = fullName.last match {
    case '$' => mirror.reflectModule(mirror.staticModule(fullName.substring(0, fullName.length() - 1))).instance.asInstanceOf[RosalindSolution]
    case _ => {
      val classSymbol = mirror.staticClass(fullName)
      val classMirror = mirror.reflectClass(classSymbol)
      val constructor = classSymbol.toType.declaration(nme.CONSTRUCTOR).asMethod
      // TODO ParallelDnaNucleotideCounter takes two arguments
      classMirror.reflectConstructor(constructor).apply().asInstanceOf[RosalindSolution]
    }
  }

  /**
   * Gets an instance of a Scala class/singleton object by its fully qualified name. Instances
   * are either pulled from a cache or discovered using reflection.
   *
   * @param fullName The fully qualified Java class name
   * @return An instance of [[com.mattwittmann.lambdalounge.languageshootout2013.RosalindSolution]]
   */
  def getInstance(fullName: String): RosalindSolution =
    if (cached.contains(fullName))
      cached(fullName)
    else
      (cached += ((fullName, getInstanceByReflection(fullName))))(fullName)

  val cached = Map[String, RosalindSolution]()
  val mirror = runtimeMirror(getClass.getClassLoader)
  Source.fromInputStream(getClass.getResourceAsStream("/data.csv")).getLines.foreach { line =>
    val fields = line.split(",")
    if (fields.length == 3 && fields(0) != "Class") {
      test(line) {
        val instance = getInstance(fields(0))
        val input = fields(1)
        val expected = fields(2)
        val output = instance.mkString(input)
        val start = System.currentTimeMillis()
        for (i <- 0 to times) {
          instance.mkString(input)
        }
        val end = System.currentTimeMillis()
        info(s"in ${end - start} ms/${times}")
        assert(expected == output)
      }
    }
  }
}