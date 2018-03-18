/** **********   Lecture 4.3 - Subtyping and Generics   *************/
//Type bounds
//def assertAllPos(r: IntSet): IntSet (returns r if all elems are positive, or exeption)
//def assertAllPos[S <: IntSet](r:S): S

//Covariance

//todo: read    A <: B      A is a B          (A is a subtype, B is a supertype or "base")
//The liskov substitution principle: if A <: B, then everything one can
// do with a value of type B, one should also be able to do with a value of type A


/** **********   Lecture 4.4 - Variance   *************/
//List should be covariant, where Array should not. This is because List is
//immutable, and Array is not.

//immutable types can be variant when some conditions are true

//example:
//type A = IntSet => NonEmpty
//type B = NonEmpty => IntSet
//                            turns out that A <: B (A is a B)
// anything you can do with with a B (give a NonEmpty and get a IntSet),
// you can do with a A (also give a NonEmpty and get a IntSet).

//generally, we have the rule for subtyping between function types:
//
//If A2 <: A1 and B1 <: B2, then
//
// A1 => B1 <: A2 => B2
//
//
// (A1 => B1) accepts the broad case (both A types) and returns the narrow case (just B1)

//This (^) means that functions are contravariant in their argument types and
//covariant in their result types
//
//package scala
//trait Function1[-T, +U]{
//  def apply(x: T): U
//}

//there are rules to when you can sprinkle +/-, and scalac checks for this. they
//are related to weather the type is of arguments or results.

//lets implement list with the new things we've learned:

trait List[+T] {

    def isEmpty: Boolean

    def head: T

    def tail: List[T]

    // this does not work - elem is covariant in a contravariant position
    //    def prepend(elem: T) : List[T] = new Cons(elem, this)
    //
    //also this would break the contract:
    //given a List[IntSet], a List[NonEmpty] is then <: List[IntSet]
    //
    //but calling .prepend(Empty) on a List[IntSet] works, while
    // calling .prepend(Empty) on a List[NotEmpty] leads to a type error
    // thus braking the contract for the subtype List[NonEmpty]

    def prepend[U >: T](elem: U) : List[U] = new Cons(elem, this)
    //now prepend returns a list of the supertype, and allows for elem to be Empty as subtype of IntSet
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {

    override def isEmpty = false
}

//Nil becomes a singleton list of Nothing (Nothing <: Everything else)
object Nil extends List[Nothing] {

    override def isEmpty: Boolean = true

    override def head: Nothing = throw new NoSuchElementException("Nil.head")

    //Nothing is a subtype of every type, therefore also of List[Nothing]
    override def tail: Nothing = throw new NoSuchElementException("Nil.head")
}

object List {

    def apply[T](): List[T] = Nil

    def apply[T](x1: T): List[T] = new Cons(x1, List())

    def apply[T](x1: T, x2: T): List[T] = new Cons(x1, List(x2))

}


val x: List[String] = Nil
println(x)

