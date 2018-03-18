/** **********   Lecture 4.6 - Lists   *************/
//2 diffs between lists and arryas:
// 1. lists are immutable
// 2. lists are recursive (head | tail), arrays are flat

//all lists are constructed from the empty list, Nil, and :: (cons)
val fruits = List("apples", "oranges", "pears")
val fruits2 = "apples" :: ("oranges" :: ("pears" :: Nil))
val empty = Nil

//CONVENTION: operators ending in ":" associate to the right
// A :: B :: C is interpreted as  A :: (B :: C)
val fruits3 = "apples" :: "oranges" :: "pears" :: Nil
//CONVENTION: operators ending in ":" are seen as method calls to the right operand
val nums = Nil.::(4).::(3).::(2).::(1)


//sorting list (O(n^2))
def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => List(x)
  case y :: ys => if(x <= y) x :: xs else y :: insert(x, ys)
}