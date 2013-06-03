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
 *
 * @param times The number of times to run each test case
 */
class TestHarness(val times: Int) extends FunSuite {
  /** Defaulting to run 200 times. sbt needs an explicit zero-argument constructor for a ScalaTest Suite. */
  def this() = this(200)

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

  /**
   * Fixing expected if it contains, for example, a fraction.
   *
   * @param expected The original expected string from the CSV file
   * @return A fixed-up version of expected
   */
  def fixExpected(expected: String): String =
      if (expected.contains("/")) {
        val split = expected.split("/")
        val numerator = java.lang.Double.parseDouble(split(0))
        val denominator = java.lang.Double.parseDouble(split(1))
        java.lang.Double.toString(numerator / denominator)
      }
      else
        expected

  val cached = Map[String, RosalindSolution]()
  val mirror = runtimeMirror(getClass.getClassLoader)
  Source.fromInputStream(getClass.getResourceAsStream("/data.csv")).getLines.foreach { line =>
    val fields = line.split(",")
    if (fields.length == 3 && fields(0) != "Class") {
      test(line) {
        val instance = getInstance(fields(0))
        val input = fields(1)
        val expected = fixExpected(fields(2))
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