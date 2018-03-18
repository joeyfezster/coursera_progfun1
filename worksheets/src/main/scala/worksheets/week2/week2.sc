import scala.math.abs

object excersize {
  /** **********   Lecture 2.2 - Currying   *************/
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)

  product(x => x * x)(3, 4)

  def fact(a: Int) = product(x => x)(1, a)

  fact(5)

  def genSeries(step: (Int, Int) => Int, unit: Int)(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) unit
    else step(f(a), genSeries(step, unit)(f)(a + 1, b))

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int =
    if (a > b) unit
    else combine(f(a), mapReduce(f, combine, unit)(a + 1, b))

  //define prod in terms of mapReduce:
  def prod(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x, y) => x * y, 1)(a, b)

  def factNew(a: Int) = prod(x => x)(1, a)

  factNew(5)

  /** **********   Lecture 2.3 - Finding Fixed Points   *************/
  val tolerance = 0.00000001

  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x / 2)(1)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  //does not converge without the averageDamp, the guess keeps oscillating from 1.0 and 2.0
  def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1)


  /** **********   Lecture 2.5 - Functions and Data   *************/
  /** **********   Lecture 2.6 - More Fun with Rationals   *************/

  //in scala there's a default constructor that just executes the class body

  class Rational(x: Int, y: Int) {
    //protect against zero division
    require(y != 0, "denominator must not be zero")

    //secondary constructor:
    def this(x: Int) = this(x, 1)

    //let's use gcd to make sure we simplify the rational at construction:
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    //could also define numer and denum as functions using "def" instead of "val", downside would be that they will
    // compute every time they are called
    val numer = x / gcd(x, y)
    val denum = y / gcd(x, y)

    //Let's define arithmetic over rationals
    def add(that: Rational) =
      new Rational(
                    numer * that.denum + that.numer * denum,
                    denum * that.denum)

    def neg =
      new Rational(-numer, denum)

    def substract(that: Rational) = add(that.neg)

    override def toString = numer + "/" + denum

    def less(that: Rational) = this.numer * that.denum < that.numer * this.denum

    def max(that: Rational) = if (this.less(that)) that else this
  }

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)
  x.substract(y).substract(z)
  x.less(y)

  /** **********   Lecture 2.7 - Evaluation and Operators   *************/
  // lets change the operator functions to proper operators
  class Rational2(x: Int, y: Int) {
    println(s"num: $x, denom: $y, gcd: ${gcd(x,y)}")
    require(y > 0, "denominator must be positive")

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    val g = abs(gcd(x,y))

    val numer = x / g
    val denum = y / g

    def + (that: Rational2) =
      new Rational2(this.numer * that.denum + that.numer * this.denum,
                    this.denum * that.denum)

    //note that in scala unary_* defines a unary operator * on this object:
    //unary_- is used as simply as "-t"
    def unary_- : Rational2 = new Rational2(-this.numer, this.denum)

    def - (that: Rational2) = this + -that

    override def toString = this.numer + "/" + this.denum

    def < (that: Rational2) = this.numer * that.denum < that.numer * this.denum

    def max(that: Rational2) = if (this < that) that else this
  }

  val x2 = new Rational2(1, 3)
  val y2 = new Rational2(5, 7)
  val z2 = new Rational2(3, 2)

  x2 - y2 -z2

}
