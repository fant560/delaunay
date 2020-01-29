package ru.udsu.algs.utils

import org.scalatest.flatspec.AnyFlatSpec

import scala.language.postfixOps

class TriangleSpec extends AnyFlatSpec {

  val a: Point = Point(1.0, 1.0)
  val b: Point = Point(4.0, 2.0)
  val c: Point = Point(3.0, 5.0)

  val triangle: Triangle = Triangle(a, b, c)

  val containingPoint: Point = Point(3.5, 2.5)
  val notContainingPoint: Point = Point(3.5, 1.5)

  "Triangle containing method" should "work correctly" in {
    assert(triangle containing containingPoint)
    assert(!(triangle.containing(notContainingPoint)))
  }

  "Triangle is oriented counter clockwise" should "work correctly" in {
    assert(triangle isOrientedCounterClockWise)
    assert(!Triangle(c, b, a).isOrientedCounterClockWise)
  }

  "Triangle is point in curcumcircle" should "work correctly" in {
    assert(triangle.isPointInCircumcircle(notContainingPoint))
    assert(!triangle.isPointInCircumcircle(Point(0.5, 0.5)))
  }

  "Triangle contains edge" should "work correctly" in {
    assert(triangle.containsEdge(Edge(a, b)))
    assert(triangle.containsEdge(Edge(c, a)))
    assert(!triangle.containsEdge(Edge(c, notContainingPoint)))
  }

  "Triangle get not containing edge point" should "work correctly" in {
    assert(triangle.getNoneEdgeVertex(Edge(a, b)) == c)
    assert(triangle.getNoneEdgeVertex(Edge(c, b)) == a)
  }

  "Triangle is triangle point" should "work correctly" in {
    assert(triangle.containing(a))
    assert(!triangle.containing(notContainingPoint))
  }

  "Triangle find nearest edge" should "work correctly" in {
    assert(triangle.findNearestEdge(notContainingPoint)._1 == Edge(a, b))
    assert(triangle.findNearestEdge(Point(1.5, 3.5))._1 == Edge(c, a))
    assert(triangle.findNearestEdge(Point(4.0, 4.0))._1 == Edge(b, c))

  }
}
