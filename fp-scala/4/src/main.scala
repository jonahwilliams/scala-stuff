trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None     =>  None
      case Some(x)  =>  Some(f(x))
    }
    def flatMap[B](f: A => Option[B]): Option[B] =
      this.map(f).getOrElse(None:Option[B])

    def getOrElse[B >: A](default: => B): B = this match {
      case None     => default
      case Some(x)  => x
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] =
      this.map(Some(_)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
      this.flatMap(x => if (f(x)) Some(x) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



object main extends App {

  // mean is an example of a partial function: it's not defined for some inputs.
  // A function is partial when it makes some assumptions about the inputs
  // that aren't implied by its input types
  // def mean(xs: Seq[Double]): Double =
  //   if (xs.isEmpty)
  //     throw new ArithmeticException("mean of empty list!")
  //   else xs.sum / xs.length
  //
  //
  // // doesn't give us options ... options
  // def mean2(xs: Seq[Double], onEmpty: Double): Double =
  //   if (xs.isEmpty) onEmpty
  //   else xs.sum / xs.length


  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t) map(hh :: _))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)) )

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil      => Some(Nil)
      case h :: t   => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)



  ///////


  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def insuranceRateQuote(age: Int, tickets: Int): Int = age * tickets + 10


  def parseInsuranceRateQuote(
      age: String,
      numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

}
