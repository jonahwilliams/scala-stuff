class Rational(n: Int, d: Int) {

  require(d != 0)

  private val g = gcd(n.abs, d.abs)

  val numer: Int = n / g
  val denom: Int = d / g

  // secondary constructor
  def this(n: Int) = this(n, 1)

  override def toString() =
    numer + "/" + denom

  /*
    Methods on (Rational, Rational)
  */
  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def * (that: Rational): Rational =
    new Rational(
      numer * that.numer,
      denom * that.denom
    )

  def / (that: Rational): Rational =
    new Rational(
      numer * that.denom,
      denom * that.numer
    )

  def - (that: Rational): Rational =
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )


  // Methods on (Rational, Int)
  def + (i: Int): Rational =
    new Rational(numer + i * denom, denom)

  def - (i: Int): Rational =
    new Rational(numer - i * denom, denom)

  def * (i: Int): Rational =
    new Rational(numer * i, denom)

  def inverse(): Rational =
    new Rational(
      denom,
      numer
    )
  /*
    this is uneccesary here
  */
  def lessThan(that: Rational): Boolean =
    this.numer * that.denom < this.denom * that.numer

  def max(that: Rational): Rational =
    if ( lessThan(that) ) that else this

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}


object Test {
  def main(args: Array[String]): Unit = {
    val a = new Rational(2, 3)
    println(a)
    val b = new Rational(1, 4)
    println(b)

    val c = a * b - a / b

    println(c)
  }
}
