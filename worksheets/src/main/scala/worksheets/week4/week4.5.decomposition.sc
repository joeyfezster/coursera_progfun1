/** **********   Lecture 4.5 - Decomposition   *************/

trait Expr {

    //    //classification (leads to quad growth)
    //    def isNumber: Boolean
    //    def isSum: Boolean
    //    //accessor (leads to quad growth)
    //    def numValue: Int
    //    def leftOp: Expr
    //    def rightOp: Expr
    //what happens if now you want to add new expressions:
    //Product, Variable (x), keeping the classification method structure, you get quadratic
    //growth in the number of methods to implement (just adding these 2 causes 25 methods
    // to be added)

    //OO solution
    def eval: Int

    //how do you show expressions?
    //would have to def a show :String function, that now
    //needs to be implemented in all subtypes (not good)

    //problem:
    //how do we now simplify expressions? (a*b + a*c -> a*(b+c))
    //can't do this with current design, not a local simplification, so we're back
    //at sq one
}

class Number(n: Int) extends Expr {

    override def eval = n
}

class Sum(e1: Expr, e2: Expr) extends Expr {

    override def eval = e1.eval + e2.eval
}

// //not so good, uses original now not existing classification methods
//def eval(e: Expr): Int = {
//    if(e.isNumber) e.numValue
//    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
//    else throw new Error("i don't know this")
//}

////unsafe typecast solution: (typecast can fail and throw exception in RT)
//def eval(e: Expr): Int = {
//    if (e.isInstanceOf[Number]) e.asInstanceOf[Number].numValue
//    else if (e.isInstanceOf[Sum]) eval(e.asInstanceOf[Sum].leftOp) + eval(e.asInstanceOf[Sum].rightOp)
//    else throw new Error("Unknown expression " + e)
//}