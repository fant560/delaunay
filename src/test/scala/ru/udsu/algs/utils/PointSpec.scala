package ru.udsu.algs.utils

import org.scalatest.flatspec.AnyFlatSpec

class PointSpec extends AnyFlatSpec {

  val point1: Point = Point(1.0, 2.0)
  val point2: Point = Point(4.0, 6.0)

  "A point to point multiplication" should "work correctly" in {
    // 1 * 4 + 2 * 6
    val result = 16.0
    assert(point1 * point2 == result)
  }

  "A point to scalar multiplication" should "work correctly" in {
    val result = Point(4.0, 8.0)
    assert(point1 * 4.0 == result)
  }

  "A point addition" should "work correctly" in {
    val result = Point(5.0, 8.0)
    assert(point1 + point2 == result)
  }

  "A point subtraction" should "work correctly" in {
    val result = Point(3.0, 4.0)
    assert(point2 - point1 == result)
  }

  "A point norm" should "work correctly" in {
    val result = 5.0
    assert(Point(3.0, 4.0).norm() == result)
  }

  "A point cross" should "work correctly" in {
    val result = 2.0
    assert(point1.cross(point2) == result)
  }

  "A point to string" should "be informative" in {
    println(point1.toString)
  }
}
