
/** **********   Lecture 3.1 - Class Hierarchies   *************/
println("welcome to the scala worksheet")

abstract class IntSet {

    def incl(x: Int): IntSet

    def contains(x: Int): Boolean

    def union(other: IntSet): IntSet
}

//let's take the aproach of a sorted binary tree as an int set impl
//todo// using "object "instead of "class" defines Empty as a singleton object
//todo// as such, it does not require evaluatoin, as it evaluates to itself,
//todo// and you can't call "new" for it
object Empty extends IntSet {

    override def incl(x: Int) = new NonEmpty(x, Empty, Empty)

    override def contains(x: Int) = false

    override def toString = "."

    override def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

    override def incl(x: Int) =
        if (x < elem) new NonEmpty(elem, left.incl(x), right)
        else if (x > elem) new NonEmpty(elem, left, right.incl(x))
        else this

    override def contains(x: Int) =
        if (x < elem) left.contains(x)
        else if (x > elem) right.contains(x)
        else true

    override def toString: String = "{" + left + elem + right + "}"

    override def union(other: IntSet): IntSet = {
        left.union(right).union(other).incl(elem)
    }
}

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = new NonEmpty(4, Empty, Empty)
val t3 = t1.incl(5)
val u1 = t1.union(t2)

/** **********   Lecture 3.2 - How Classes are Organized   *************/
//martin shows differen forms of import: specific, {list, of, parts}, ._ everything

// traits are like interfaces in java (but can contain fields and concrete methods)
// main diff between trait and class is traits cannot contain value parameters

//every scala program automatically imports:
//import scala
//import java.lang
//import scala.Predef

//scala.AnyRef is an alias for java.lang.Object

//type scala.Nothing and scala.Null (Null inherits from all refs, Nothing inherits from everything)
//Martin shows how to throw exceptions

/** **********   Lecture 3.3 - Polymorphism   *************/
//type parameterization - when the type is a parameter
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

//Polymorphism - from greek "having many forms"
// polymorph functions -> can be applied to many types
// polymorph classes or types -> can be instantiated with many types

//two forms seen here are 1. subtyping (Cons and Nil extend List) and 2. generics List[T]

def readNth[T](list: List[T], n: Int): T = {
    if (list.isEmpty || n < 0) throw new IndexOutOfBoundsException
    if (n == 0) list.head
    else readNth(list.tail, n - 1)
}
val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
readNth(list, 2)
//readNth(list, 4)
//readNth(list, -1)
readNth(list, 0)


/** **********   Assignment - Scala Syntax   *************/
def makeSeq(): Seq[Int] = {
    for (n <- Seq(1, 2, 3)) yield {
        n + 1
    }
}
makeSeq


