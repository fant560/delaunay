package ru.udsu.algs.utils

case class Point(x: Double, y: Double) {
  def +(other: Point): Point = Point(x + other.x, y + other.y)

  def -(other:Point): Point = Point(x - other.x, y - other.y)

  def *(scalar: Double) : Point = Point(x * scalar, y * scalar)

  def norm() : Double = Math.sqrt(x * x + y * y)


  def *(other: Point): Double = x * other.x + y * other.y
  /**
   * Псевдоскалярное произведение
   * @param other точка
   * @return значение
   */
  def cross(other: Point) : Double = y * other.x - x * other.y

  override def toString: String = s"Vector[$x, $y]"
}
