package ru.udsu.algs.utils

import scala.collection.mutable

class TriangulationContainer(
                              val triangles: mutable.ListBuffer[Triangle]
                            ) {


  def add(triangle: Triangle): Unit = {
    triangles += triangle
  }

  def remove(triangle: Triangle): Unit = {
    triangles -= triangle
  }

  /**
   * Найти первый треугольник, содержащий переданную точку
   *
   * @param point точка
   * @return треугольник или null, если не нашли
   */
  def findContainingTriangle(point: Point): Triangle = {
    triangles.find(_ containing point).orNull
  }

  /**
   * Найти соседний треугольник, разделяющий сторону
   *
   * @param triangle треугольник
   * @param edge     сторона
   * @return сосед или null
   */
  def findNeighbour(triangle: Triangle, edge: Edge): Triangle = {
    triangles.find(tr => (tr containsEdge edge) && (tr ne triangle)).orNull
  }

  /**
   * Найти первый треугольник, содержащий сторону
   *
   * @param edge сторона
   * @return треугольник или ничего
   */
  def findTriangleContainingEdge(edge: Edge): Triangle = {
    triangles.find(_ containsEdge edge).orNull
  }

  /**
   * Найти сторону, ближающую к переданной точке среди треугольников
   *
   * @param point точка
   * @return сторона
   */
  def nearestEdge(point: Point): (Triangle, Edge) = {
    val minimumTuple = triangles.map(tr => (tr, tr findNearestEdge point)).minBy(_._2._2)
    (minimumTuple._1, minimumTuple._2._1)
  }

  /**
   * Удалить все треугольники содержащие вершину
   *
   * @param point вершина
   */
  def removeTrianglesUsing(point: Point): Unit = {
    triangles --= triangles.filter(_ isTrianglePoint point)
  }
}
