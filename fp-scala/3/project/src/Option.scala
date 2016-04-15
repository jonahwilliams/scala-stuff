trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None     => None:Option[B]
    case Some(x)  => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None     => None:Option[B]
    case Some(x)  => f(x)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None     => ob
    case Some(x)  => Some(x)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x)  => Some(x)
    case _                => None:Option[A]
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object List {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    val d = xs.map(x => m.map(y => Math.pow(x - y, 2)))
    val c = d.map(x => x.getOrElse(0.0))
    mean(c)
  }

  def main(args: Array[String]): Unit = {
    println(variance(Seq(1.0, 2.0, 3.0)))
  }
}
