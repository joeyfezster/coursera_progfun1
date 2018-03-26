/** **********   Lecture 5.1 - More Functions on List   *************/

//martin mentions more list functions such as xs.last, xs.init, ++ (concat), reverse, updated, indexof, contains..
//to better understand list methods, we implement some:

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}


//xs:::ys = ys.:::(xs)
//pattern match on xs cuz it's the first elements of the result
def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}
//complexity if |xs|

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => reverse(ys) ++ List(y)
}
//complexity is n*n, and we'll see a better implementation later


def removeAt[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n + 1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case (y: List[Any]) :: ys => flatten(y) ++ flatten(ys)
  case y :: ys => y :: flatten(ys)
}


/** **********   Lecture 5.2 - Pairs and Tuples   *************/
//this section we learn how to paris and tuples can help in program composition and decomposition

//a tuple in scala is the parameterized type scala.TupleN[T1, ..., TN]
//a tuple expression (e1,...,eN) is equivalent to the function application scala.TuppleN(e1,...,eN)
//a tuple pattern (p1,...,pN) is equivalent to the consturctor scala.TupleN(p1,...,pN)

//for example, pair is modeled like so:
case class Tuple2[T1, T2](_1: T1, _2: T2) {
  //for nicer printing:
  override def toString = "(" + _1 + "," + _2 + ")"
}

//let's see a non-trivial example: sorting
def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    //todo: see how splitAt(n) returns a pair:
    val (fst, snd) = xs splitAt n
    merge1(msort(fst), msort(snd))
  }
}

def merge1(xs: List[Int], ys: List[Int]): List[Int] = {
  xs match {
    case Nil => ys
    case x :: xs1 => ys match {
      case Nil => xs
      case y :: ys1 => if (x < y) x :: merge1(xs1, ys) else y :: merge1(xs, ys1)
    }
  }
}

//todo: better merge, using pair pattern matching
def merge2(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
  case (xs, Nil) => xs
  case (Nil, ys) => ys
  case (x :: xs1, y :: ys1) => if (x < y) x :: merge1(xs1, ys) else y :: merge1(xs, ys1)
}

val nums = List(2, 5, -1, 3, 4)
msort(nums)

/** **********   Lecture 5.3 - Implicit Parameters   *************/
//Problem: we would like to parameterize msort, so that it can be used for any
//type of elements
// but def msort[T](...) wouldn't work, since < is not defined for an arbitrary type T
//Idea: parameterize merge with the < function

def msortParam[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def mergeParam(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x :: xs1, y :: ys1) =>
        if (lt(x, y)) x :: mergeParam(xs1, ys)
        else y :: mergeParam(xs, ys1)
    }

    val (fst, snd) = xs splitAt n
    mergeParam(msortParam(fst)(lt), msortParam(snd)(lt))
  }
}

msortParam(nums)((x: Int, y: Int) => x < y)

val fruits = List("apple", "pinaple", "orange", "banana")
msortParam(fruits)((x, y: String) => x.compareTo(y) < 0)

//we can get rid of the types in the tuple, because the scala compiler can infer them by analyzing the
//call of msortParam, since nums is a List[Int], then it figures that msortParam's parameter is Int, and that
//determines the types of the tuple
msortParam(fruits)((x, y) => x.compareTo(y) < 0)
//todo: this shows that when you have function types in your call, it's better to
//todo: add them last, this way the compiler might have already inferred them, and that shortens the
//todo: syntax

//using Ordered
//there's already a class that represents ordering, scala.math.Ordering[T]
def msortOrd[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def mergeParam(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x, y)) x :: mergeParam(xs1, ys)
        else y :: mergeParam(xs, ys1)
    }

    val (fst, snd) = xs splitAt n
    mergeParam(msortOrd(fst)(ord), msortOrd(snd)(ord))
  }
}

msortOrd(nums)(Ordering.Int)
msortOrd(fruits)(Ordering.String)


//using implicit ord
//the compiler will figure out the correct parameter to pass.
//the compiler searches an implicit definition that:
// * is marked implicit
// * has a type compatible with T
// * is visible at the point of the function call, or is defined in a companion object associated
//    with T
//if there's a single definition, it will be taken as actual argument for the implicit parameter
//otherwise it's an error
def msortImplicit[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def mergeParam(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x, y)) x :: mergeParam(xs1, ys)
        else y :: mergeParam(xs, ys1)
    }

    val (fst, snd) = xs splitAt n
    //note: we no longer need to pass the ord, because it is visible (func def)
    mergeParam(msortImplicit(fst), msortImplicit(snd))
  }
}

//here we can ommit the ord, because it is defined in companion object in scala.math.Ordering
msortImplicit(nums)
msortImplicit(fruits)

//here we'll discuss list functions that take lists and other functions to create a
//grater variaty of different tasks

//for example, if one wishes to multiply every element of a list by a factor, one could
def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y :: ys => y * factor :: scaleList(ys, factor)
}

//but we can generalize it:
//abstract class List[T] {
//
//  def map[U](f: T => U): List[U] = this match {
//    case List() => this
//    case x :: xs => f(x) :: xs.map(f)
//  }
//}

def squareList1(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y * y :: squareList1(ys)
}

def squareList2(xs: List[Int]): List[Int] = xs.map(x => x * x)

//we can also easily filter out elements:
//instead of
def posElements(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => if (y > 0) y :: posElements(ys) else posElements(ys)
}
//we can do
def posElems(xs: List[Int]): List[Int] = xs.filter(x => x > 0)

//we can see other methods:
nums.filter(x => x > 0)
nums.filterNot(x => x > 0)
nums.partition(x => x > 0) //this is like both fiter, and filter not, but they are returned as a pair

nums.takeWhile(x => x > 0) //this takes the longest prefix of the list that satisfies
nums.dropWhile(x => x > 0) //this drops elements from the top until the predicate is not met
nums.span(x => x > 0) //this is like partition, but for takewhile and dropwhile


//this function packs consecutive occurences into separate lists
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs.span(y => y == x)
    first :: pack(rest)
}

val data = List("a", "a", "a", "b", "c", "c", "a")
pack(data)

//encode would make a list of tuples (x, count), where x is the repeated value, and count is the
//times it was repeated
def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs).map(ys => (ys.head, ys.length))
}

/** **********   Lecture 5.5 - Reduction of Lists   *************/
///we introduce a new class of Fold/Reduce operators

//another common usage of lists is combining it's values, for example
//sum(List(1,2,3)) = 0 + 1 + 2 + 3
//this pattern can be abstracted using reduceLeft.

//reduceLeft inserts a given binary operator between adjacent elements of a list:
//List(x1, ..., xn).reduceLeft(op) = (...(x1 op x2) op ... ) op xn
//usig reduceLeft we can simplify
def sums(xs: List[Int]) = (0 :: xs).reduceLeft((x, y) => x + y)

//or using scala's _:
def mults(xs: List[Int]) = (1 :: xs).reduceLeft(_ * _)
//the idea behind scala's underscore (_) is every _ represents a new param, going from left to right
//the params are defined at the next outer pair of parentheses (or the whole expression if there are none)

//since reduceLeft can only be used on non empty lists, we have foldLeft, that takes a parameter z
//that is returned in case the list is empty (kinda like adding the 0::xs or 1::xs for you)
def sums2(xs: List[Int]) = xs.foldLeft(0)(_ + _)

////martin gives a tentative implementation of reduceLeft and foldLeft.
//abstract class List[T]{
//  def reduceLeft(op: (T,T) => T):T = this match {
//    case Nil => throw new Error("Nil.reduceLeft")
//    case x :: xs => xs.foldLeft(x)(op)
//  }
//
//  def foldLeft[U](z: U)(op: (U,T) => U): U = this match {
//    case Nil => z
//    case x :: xs => xs.foldLeft(op(z,x))(op)
//  }
//}

//there's a dual pair of operations, reduceRight, and foldRigt, that do the same, but the
//operations resolve from the right

//when the operators are associative and comutatice, foldLeft and foldRight are equivalent, but
//differ in efficiency

/** **********   Lecture 5.6 - Reasoning about Concat   *************/
//martin talks about one of the core claims of functional programing,
//namely that it is more amenable to reasoning about programs.

//thinking of concat, we can argue that concat is correct it:
// * (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
// * xs ++ Nil = xs
// * Nil ++ xs = xs

//we can use Structural induction (based on natural induction) to prove these claims
//there's an example of natural induction, with the key takeaway of:
//A proof can freely apply reduction steps as equalities to some part of a term, this works because pure
//Functional Programing has no side effects, so the reductions can be equivalent to re-writing the term
//This is called Referential Transparency
//you can view an example of Structural Induction on minute 9
//https://www.coursera.org/learn/progfun1/lecture/5scUh/lecture-5-6-reasoning-about-concat

/** **********   Lecture 5.6 - Reasoning about Concat   *************/
//view video