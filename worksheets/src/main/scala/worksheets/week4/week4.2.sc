/** **********   Lecture 4.1 - Objects Everywhere   *************/
//Martin makes a case that scala is a pure OO language, and shows how
//Boolean can be an OO class implementation - week4.1


//martin then shows an implementation of Int, that includes some overloading of +
//depending on the right operand (1 + 2.0 should return a double, 1 + L2 should return long)


//quiz, implement natural numbers using no primitives at all (booleans can also be impl without primitives):
//peano numbers: using these, we can implement integers, floats and the works
abstract class Nat{
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)
    def + (that: Nat): Nat
    def - (that: Nat): Nat
}

object Zero extends Nat{

    override def isZero = true

    override def predecessor = throw new Error("0.predecessor")

    override def +(that: Nat) = that

    override def -(that: Nat) = if(that.isZero) this else throw new Error("negative num")
}

class Succ(n: Nat) extends Nat{

    override def isZero = false

    override def predecessor = n

    override def +(that: Nat) = new Succ(n + that)

    override def -(that: Nat) = if(that.isZero) this else n - that.predecessor
}

/** **********   Lecture 4.2 - Functions as Objects   *************/
//Functions are already treated as objects, for example, the type A => B is just an
//abbreviation of scala.Function[A,B].

//on the other hand, an anonymous function such as (x: Int) => x*x
//is expanded using an anonymous class.

//methods defined with 'def' are not objects themselves (that would create an infinite
// expansion loop, i.e. def apply()). Instead, when the identifier is used elsewhere
// in the code, it is converted to the function value or expanded as anonymous class
// this is called an eta - expansion eta expansion.
object week4_2 {

    trait List[T] {

        def isEmpty: Boolean

        def head: T

        def tail: List[T]
    }

    //note how "val" defines head and tail as legal implementations of the trait methods
    //in scala val is a special case of methods, and they can override methods
    //the difference is that val is evaluated during the object's init, and def is evluated every time
    class Cons[T](val head: T, val tail: List[T]) extends List[T] {

        override def isEmpty = false
    }

    class Nil[T] extends List[T] {

        override def isEmpty: Boolean = true

        //Nothing is a subtype of every type, therefore also of T
        override def head: Nothing = throw new NoSuchElementException("Nil.head")

        override def tail: Nothing = throw new NoSuchElementException("Nil.head")
    }

    object List {

        def apply[T](): List[T] = new Nil

        def apply[T](x1: T): List[T] = new Cons(x1, List())

        def apply[T](x1: T, x2: T): List[T] = new Cons(x1, List(x2))

    }

}


