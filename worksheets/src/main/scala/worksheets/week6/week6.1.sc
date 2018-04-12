/** **********   Lecture 6.1 - Other Collections   *************/
object week6_1 {
  //martin talks about vector, a collection with more evenly balanced access patterns than list
  //the implementation of vector varies according to how many items it contains, but in essence it's a
  //very shallow tree.
  //
  //up to 32 items, it's simply an array of those items,
  //after that, the original array becomes an array of 32 pointers to 32 arrays of items (32^2 items)
  //when that's exhausted, the implementation changes again, and a new layer of arrays is created (32^3 items)
  //and so on..
  //
  //in this case, the depth of vector is log32(N), and so is the access time to each element.
  //
  //vectors are recommended for bulk operations, and for random access requirements.
  //this is further improved upon lists since the size and locality of the vector's array, means that
  //the following items are likely to be cashed together, unlike list cons.
  //
  //on the other hand, if you access patterns are recursive, lists are recommended since access to head
  //an tail are constant


  //List and Vector are subtypes of Seq, which is a subtype of Iterable

  //other subtypes of Iterable are Map, and Set.

  //Array, and String are (kind of) also subtypes of Seq, so map, fold, filter (etc) are all
  //methods of those also.


  //Range
  //Range is also a subtype of Seq, and it's api can be very useful
  val r1: Range = 1 until 5 //1,2,3,4 - 5 is excluded
  val r2: Range = 1 to 5 //1,2,3,4,5 - 5 is inclusive
  1 to 10 by 3 //use of by (step) operator
  6 to 1 by -2 //negative step

  //
  //ranges are compactly stored: lower bound, upper bound, and step.

  //looking at some more operations of Seq:

  //exists(predicate) - true if the predicate hods for at least one element
  //
  //forall(predicate) - true if the predicate holds for all elements in the seq
  //
  //xs zip ys - a seq of pairs corresponding to elements of each seq (size of smallest)
  //
  //unzip - returns a pair of 2 lists, each containing the elements of the original lists

  val xs = Array(1, 2, 3, 44)
  val hello = "Hello World"

  xs map (x => x * 2)

  hello filter (_.isUpper)

  hello exists (_.isUpper)

  hello forall (_.isLower)

  val pairs: Array[(Int, Char)] = xs zip hello

  val orig: (Array[Int], Array[Char]) = pairs.unzip

  orig._2.foreach(print)

  //flatmap(func) applies func (a function from the original elements to sequences) to each element
  //              and then it concatenates the resulting sequences into one

  //then there's some methods for numeric seqs: sum, max, min
  List(4, 2, 8, -78, 8937, 3).max

  //example: Combinations:
  1 to 5 flatMap (x => 10 to 15 map (y => (x, y)))

  //example: scalar product:
  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum

  //alternative impl:
  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map { case (x, y) => x * y }.sum


  //Exercise (value conciseness over efficiency):
  def isPrime(n: Int): Boolean = 2 until n forall  (x => n % x != 0)

  isPrime(17)
}

