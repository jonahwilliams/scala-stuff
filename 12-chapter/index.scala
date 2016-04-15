trait Philosophical {
  def philosophize(): Unit = {
    println("I consume memory, therefor I am")
  }
}

trait HasLegs {
  def legs: Int = 4
}

class Animal

class Frog extends Animal with Philosophical with HasLegs {
  override def toString = "green"
}

// For example, implementing a rectangle in terms of points

class Point(val x: Int, val y: Int)

class Rectangle(val topLeft: Point, val bottomRight: Point){
  def left = topLeft.x
  def right = bottomRight.x
  def width = right - left
}

// leads to a more general class, but with repeated code

abstract class Component extends Rectangular

// implement an enrichment trait with conrete methods defined in terms
// of abstract ones
trait Rectangular {
  def topLeft: Point
  def bottomRight: Point
  def left = topLeft.x
  def right = bottomRight.x
  def width = right - left
}



// The Ordered Trait

class Rational(n: Int, d: Int) extends Ordered[Rational] {
  def numer = n
  def denom = d

  def eq(that: Rational) =
    this.compare(that) == 0

  def compare(that: Rational) =
    (this.numer * that.denom) - (that.numer * this.denom)
}

object Chapter extends App {
  val r1 = new Rational(8, 16)
  val r2 = new Rational(3, 4)
  val r4 = new Rational(8, 16)
  println(r1 > r2)
  println(r4 eq r1)
}
