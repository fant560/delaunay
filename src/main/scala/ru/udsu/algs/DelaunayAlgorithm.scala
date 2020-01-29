package ru.udsu.algs

import ru.udsu.algs.utils.{TriangulationContainer, Edge, Point, Triangle}

import scala.collection.mutable.ListBuffer

class DelaunayAlgorithm(
               val points: ListBuffer[Point],
               var triangulationContainer: TriangulationContainer = new TriangulationContainer(new ListBuffer[Triangle]())
             ) {


  def triangulation(): Unit = {
    triangulationContainer = new TriangulationContainer(new ListBuffer[Triangle]())

    /**
     * Для первых переданных точек, нормально что здесь кидается исключение
     */
    if (points == null || points.size < 3) {
      throw new IllegalStateException("Недостаточно точек, должно быть как минимум 3")
    }

    /**
     * Растянем немного координаты и засунем все в большой треугольник
     */
    val maxCoordinate = points.map(point => Math.max(point.x, point.y)).max * 16.0d

    val p1 = Point(0.0, 3.0 * maxCoordinate)
    val p2 = Point(3.0 * maxCoordinate, 0.0)
    val p3 = Point(-3.0 * maxCoordinate, -3.0 * maxCoordinate)

    val superTriangle = Triangle(p1, p2, p3)

    triangulationContainer.add(superTriangle)

    /**
     * Для каждой переданной  точки
     */
    for (i <- points.indices) {
      /**
       * Для текущей триангуляции найти первый треугольник содержащий вершину.
       * Для первой итерации всегда не null, тк большой треугольник гарантировано покрывает все вершины
       */
      val triangle = triangulationContainer.findContainingTriangle(points(i))
      /**
       * Если треугольник не найшли, значит точка не внутри текущей триангуляции
       */
      if (triangle == null) {
        /**
         * Ближайшая сторона среди всех треугольников текущей триангуляции и содержащий ее треугольник
         */
        val (first,edge) = triangulationContainer.nearestEdge(points(i))
        /**
         * Сосед первого треугольника, содержащего вершину
         */
        val second = triangulationContainer.findNeighbour(first, edge)
        /**
         * Вершины обоих треугольников, не содержащих вершины
         */
        val firstNoneEdgeVertex = first.getNoneEdgeVertex(edge)
        val secondNoneEdgeVertex = second.getNoneEdgeVertex(edge)

        /**
         * Удалим треугольники
         */
        triangulationContainer.remove(first)
        triangulationContainer.remove(second)
        /**
         * Составим новые треугольники, используя точку и найденные вершины, не имеющие сторону
         */
        val t1 = Triangle(edge.a, firstNoneEdgeVertex, points(i))
        val t2 = Triangle(edge.b, firstNoneEdgeVertex, points(i))
        val t3 = Triangle(edge.a, secondNoneEdgeVertex, points(i))
        val t4 = Triangle(edge.b, secondNoneEdgeVertex, points(i))

        /**
         * Добавим в триангуляцию
         */
        triangulationContainer.add(t1)
        triangulationContainer.add(t2)
        triangulationContainer.add(t3)
        triangulationContainer.add(t4)

        /**
         * Добавим получившиеся ребра
         */
        appendEdgeToTriangulation(t1, Edge(edge.a, firstNoneEdgeVertex), points(i))
        appendEdgeToTriangulation(t2, Edge(edge.b, firstNoneEdgeVertex), points(i))
        appendEdgeToTriangulation(t3, Edge(edge.a, secondNoneEdgeVertex), points(i))
        appendEdgeToTriangulation(t4, Edge(edge.b, secondNoneEdgeVertex), points(i))

      } else {
        /**
         * Точка внутри треугольника, значит треугольник нужно разбить и перестроить триангуляцию
         */

        val a = triangle.a
        val b = triangle.b
        val c = triangle.c

        triangulationContainer.remove(triangle)

        val first = Triangle(a, b, points(i))
        val second = Triangle(b, c, points(i))
        val third = Triangle(c, a, points(i))

        triangulationContainer.add(first)
        triangulationContainer.add(second)
        triangulationContainer.add(third)

        /**
         * Добавить ребра в триангуляцию
         */
        appendEdgeToTriangulation(first, Edge(a, b), points(i))
        appendEdgeToTriangulation(second, Edge(b, c), points(i))
        appendEdgeToTriangulation(third, Edge(c, a), points(i))
      }

    }
    triangulationContainer.removeTrianglesUsing(superTriangle.a)
    triangulationContainer.removeTrianglesUsing(superTriangle.b)
    triangulationContainer.removeTrianglesUsing(superTriangle.c)
  }

  /**
   * Добавить ребро в триангуляцию
   * @param triangle треугольник с ребром
   * @param edge ребро
   * @param point точка, которая была добавлена к триангуляции
   */
  def appendEdgeToTriangulation(triangle: Triangle, edge: Edge, point: Point): Unit = {
    /**
     * Соседний треугольник, содержащий эту же сторону
     */
    val neighbourTriangle = triangulationContainer.findNeighbour(triangle, edge)

    if (neighbourTriangle != null) {
      /**
       * Если точка попадает в описанную над соседним треугольником окружность
       */
      if (neighbourTriangle.isPointInCircumcircle(point)) {
        /**
         * Удалить оба
         */
        triangulationContainer.remove(triangle)
        triangulationContainer.remove(neighbourTriangle)

        /**
         * Составить 2 новых треугольника на вершине, которая лежит не на переданном ребре
         */
        val noneEdgeVertex = neighbourTriangle.getNoneEdgeVertex(edge)

        val firstTriangle = Triangle(noneEdgeVertex, edge.a, point)
        val secondTriangle = Triangle(noneEdgeVertex, edge.b, point)

        triangulationContainer.add(firstTriangle)
        triangulationContainer.add(secondTriangle)

        /**
         * Проверить что триангуляция не потерялась для добавленных треугольников
         */
        appendEdgeToTriangulation(firstTriangle, Edge(noneEdgeVertex, edge.a), point)
        appendEdgeToTriangulation(secondTriangle, Edge(noneEdgeVertex, edge.b), point)
      }
    }
  }


}
