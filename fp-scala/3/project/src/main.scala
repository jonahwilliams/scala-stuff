sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length[A](ns: List[A]) =
    foldLeft(ns, 0)((_, z) => z + 1)

  def reverse[A](ns: List[A]) =
    foldLeft(ns, Nil:List[A])(Cons(_, _))

  def map[A, B](ls: List[A])(f: A => B) =
    foldLeft(ls, Nil:List[B])((x, z) => Cons(f(x), z))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldLeft(l, r)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil:List[A])(append)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filter[A](ls: List[A])(f: A => Boolean): List[A] =
    ls match {
      case Nil                 => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(_, xs)         => filter(xs)(f)
    }

  def foldRight[A, B](ls: List[A], z: B)(f: (A, B) => B): B =
    ls match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldLeft[A, B](ls: List[A], z: B)(f: (A, B) => B): B =
    ls match {
      case Nil => z
      case Cons(x, xs)  => foldLeft(xs, f(x, z))(f)
    }


  def init[A](l : List[A]): List[A] = l match {
    case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
    case Cons(x, xs)  => Cons(x, init(xs))
    case Nil => Nil
  }


  def tail[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, ys)  => ys
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(x, xs) if (n > 0) => drop(xs, n - 1)
    case _ => l
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _  => l
  }

  def setHead[A](x: A, l: List[A]): List[A] = Cons(x, l)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Main extends App {

  //println(x);
  val y = List(1, 2, 3, 1, 2, 3)
  val x = List(4, 5, 6)

  val z = List.flatMap(y)(x => List(x, x))
  println(z);
}
