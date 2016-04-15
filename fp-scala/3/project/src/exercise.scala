sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int =
    fold(t)(x => 1)((l, r) => l + r + 1)

  def max(t: Tree[Int]): Int =
    fold(t)(x => x)((l, r) => Math.max(l, r))

  def depth[A](t: Tree[A]): Int =
    fold(t)(x => 1)((l, r) => Math.max(l, r) + 1 )

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)):Tree[B])(Branch(_, _))


  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x)        =>  f(x)
    case Branch(l, r)   =>  g(fold(l)(f)(g), fold(r)(f)(g))
  }
}
