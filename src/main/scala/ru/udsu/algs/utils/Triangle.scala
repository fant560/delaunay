package ru.udsu.algs.utils

case class Triangle(
                     a: Point,
                     b: Point,
                     c: Point
                   ) {
  /**
   * Лежит ли точка внутри треугольника
   *
   * @param point точка
   */
  def containing(point: Point): Boolean = {
    // по знакам псевдоскалярного произведения
    // первая ссылка в гугле
    val pab = (point - a) cross (b - a)
    val pbc = (point - b) cross (c - b)


    def sameSign(d1: Double, d2: Double): Boolean = Math.signum(d1) == Math.signum(d2)


    if (!sameSign(pab, pbc))
      return false

    val pca = (point - c) cross (a - c)
    if (!sameSign(pab, pca))
      return false

    true

  }

  /**
   * Посмотреть как расположены точки нашего треугольника.
   * В нашем случае true, если против часовой стрелки
   *
   * @return расположены ли точки в порядке против часовой стрелки
   */
  def isOrientedCounterClockWise: Boolean = {
    val a11 = a.x - c.x
    val a21 = b.x - c.x

    val a12 = a.y - c.y
    val a22 = b.y - c.y

    val det = a11 * a22 - a12 * a21
    det > 0.0d
  }

  /**
   * Проверить, лежит ли точка внутри окружности, описанной над треугольником.
   * Посчитаем исходя из знака определителя
   * a11 a12 a13
   * a21 a22 a23
   * a31 a32 a33
   * описание точек ниже
   *
   * @param point точка-претендент
   * @return лежит ли точка в описанной над треугольником окружности
   */
  def isPointInCircumcircle(point: Point): Boolean = {

    val a11 = a.x - point.x
    val a21 = b.x - point.x
    val a31 = c.x - point.x

    val a12 = a.y - point.y
    val a22 = b.y - point.y
    val a32 = c.y - point.y

    val a13 = a11 * a11 + a12 * a12
    val a23 = a21 * a21 + a22 * a22
    val a33 = a31 * a31 + a32 * a32

    val determinant = a11 * a22 * a33 + a12 * a23 * a31 + a13 * a21 * a32 -
      a13 * a22 * a31 - a12 * a21 * a33 - a11 * a23 * a32

    if (isOrientedCounterClockWise)
      determinant > 0.0d
    else
      determinant < 0.0d

  }

  /**
   * Содержится ли отрезок в треугольнике
   *
   * @param edge отрезок
   * @return содержится ли
   */
  def containsEdge(edge: Edge): Boolean = {
    (a == edge.a || b == edge.a || c == edge.a) && (a == edge.b || b == edge.b || c == edge.b)
  }

  /**
   * Вернуть точку, которая не принадлежит переданному отрезку
   *
   * @param edge отрезок
   * @return точка или null
   */
  def getNoneEdgeVertex(edge: Edge): Point = {
    if (a != edge.a && a != edge.b) {
      return a
    } else if (b != edge.a && b != edge.b) {
      return b
    } else if (c != edge.a && c != edge.b) {
      return c
    }

    null
  }

  /**
   * Принадлежит ли точка треугольнику
   *
   * @param point точка
   * @return принадлежит ли
   */
  def isTrianglePoint(point: Point): Boolean = {
    a == point || b == point || c == point
  }

  /**
   * Посчитать сторону треугольника, ближайшую к переданной точке
   *
   * @param point точка
   * @return кортеж, сторона - расстояние
   */
  def findNearestEdge(point: Point): (Edge, Double) = {
    val edges = new Array[(Edge, Double)](3)

    val ab = Edge(a, b)

    edges(0) = (ab, (computeClosestPoint(ab, point) - point).norm())

    val bc = Edge(b, c)

    edges(1) = (bc, (computeClosestPoint(bc, point) - point).norm())

    val ca = Edge(c, a)

    edges(2) = (ca, (computeClosestPoint(ca, point) - point).norm())

    edges.sortWith((e1, e2) => e1._2.compareTo(e2._2) < 0)(0)
  }

  /**
   * Хитро посчитать расстояние от стороны до точки через перпендикуляр.
   *
   * @param edge  сторона
   * @param point точка
   * @return ближайшая из переданной стороны
   */
  def computeClosestPoint(edge: Edge, point: Point): Point = {
    val ab = edge.b - edge.a

    var t = ((point - edge.a) * ab) / (ab * ab)

    if (t < 0.0d)
      t = 0.0d
    else if (t > 1.0d)
      t = 1.0d

    edge.a + (ab * t)
  }

  override def toString: String = s"Triangle a - ${a.toString()}, b - ${b.toString()}, c - ${c.toString()}"
}
