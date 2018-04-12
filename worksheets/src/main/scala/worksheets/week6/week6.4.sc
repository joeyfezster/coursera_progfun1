/** **********   Lecture 6.4 - Maps   *************/
object week6_4 {
  //Maps are special in scala because they are both Iterables and functions

  val romanNumerals: Map[Char, Int] = Map('I' -> 1, 'V' -> 5, 'X' -> 10)
  val capitalOfCountry: Map[String, String] = Map("US" -> "Washington", "Switzerland" -> "Bern")

  //maps extend the function type Key=>Value (for the Key and Value types)
  //in particular, maps can be applied to key arguments
  capitalOfCountry("US")
  //
  //but what happens when we try applying a random key:
  //  capitalOfCountry("Andorra") //NoSuchElementException
  //
  //to avoid this exception, we can use the get method of map:
  capitalOfCountry.get("Andorra")


  //The Option type
  //a simplification of the option trait:
  trait SimplifiedOption[+A]

  case class SimplifiedSome[+A](value: A) extends SimplifiedOption[A]

  object SimplifiedNone extends SimplifiedOption[Nothing]

  //since the get method in map returns an option, and that's basically a case class,
  //we can pattern match on it:
  def showCapital(country: String): String = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "missing data"
  }

  //Option also support quite a few other collection operations

  //back to collections:
  //two useful SQL-like operations: groupBy and orderBy
  //you can order a collection wity the sorted (for the natural order) or sortWith(f)
  val fruits = List("apple", "pear", "orange", "pineapple")
  fruits.sorted
  fruits.sortWith(_.length < _.length)
  //
  //groupBy is also available in scala collections. it partitions a collection into a map of
  //collections according to a discriminator function f
  val fruitsByFirstLetter: Map[Char, List[String]] = fruits.groupBy(_.head)


  //another map example:
  //a polynomial can be represented as a map, for example, x^3 -2x +5 can be
  // 0-> 5
  // 1-> -2
  // 3-> 1
  class Poly(val terms: Map[Int, Double]) {
    def +(other: Poly) = new Poly(terms ++ (other.terms).map(adjust))

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      terms.get(exp) match {
        case Some(coeff1) => exp -> (coeff + coeff1)
        case None => exp -> coeff
      }
    }

    override def toString: String = (for
      ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }

  val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))

  p1 + p2

  //The adjust function was a bit complex, so we can use withDefaultValue to
  //treat maps as full functions (so they won't fail if the key is not found)
  //also, we want to construct the polynomials without creating a map, but rather by
  //sending the exp, coeff pairs.
  class BetterPoly(terms0: Map[Int, Double]) {

    //let's define an auxiliary constructor that takes a sequence of bindings:
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    val terms = terms0 withDefaultValue 0.0

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    def oldPlus(other: BetterPoly) = new Poly(terms ++ (other.terms).map(adjust))

    //exercise: implement + with foldleft instead of ++
    //the version using foldLeft is more efficient, since it doesn't create an intermediate list
    //of bindings
    def +(other: BetterPoly) = new BetterPoly((other.terms foldLeft (terms)) (addTerm))


    //now adjust is much simpler (even if it's no longer used)
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def toString: String = (for
      ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "

  }

  val bp1 = new BetterPoly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val bp2 = new BetterPoly(0 -> 3.0, 3 -> 7.0)

  bp1 + bp2

}
