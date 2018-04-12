/** **********   Lecture 6.2 - Combinatorial Search and For-Expressions   *************/
object week6_2 {

  //high order functions can be a FP way of handling nested sequences, for example,
  //if we're trying to get all pairs (i,j) s.t. 1<=j<=i<N and i+j is prime, for some given N
  def getAllPrimeSummandPairs(limit: Int): Seq[(Int, Int)] =
    1 until limit flatMap (i => 1 to i map (j => (j, i))) filter (pair => isPrime(pair._1 + pair._2))

  def isPrime(n: Int): Boolean = 2 until n forall (x => n % x != 0)

  getAllPrimeSummandPairs(7)

  //the getAllPrimeSummandPairs function is not very readable. Let's explore another notation type
  //to help

  //Enter for expressions:
  //
  //for expressions are similar to for loops, with the important exception that they don't have
  //side effects, but rather produce a new result.
  case class Person(name: String, age: Int)

  val persons = List(Person("jane", 0), Person("joey", 29), Person("josh", 28), Person("adi", 4), Person("claribel",
    25))

  val kids: List[String] = persons filter (p => p.age < 10) map (p => p.name)
  val kids2: List[String] = for (p <- persons if p.age < 10) yield p.name

  //a for-expression is of the form
  //
  // for(pattern <- collection if filter) yield expression
  // ---|    Generator        |-| filter|
  //
  //there may be multiple generators and filters, in which case it's better to use {} in place of ()

  def getAllPrimeSummandPairsImplByForExpression(limit: Int): Seq[(Int, Int)] =
    for {
      i <- 1 until limit
      j <- 1 until i
      if isPrime(i + j)
    } yield (j, i)

  //Exercise, scalarProduct using a for expression:
  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for( (x,y) <- xs zip ys ) yield x*y).sum

}
