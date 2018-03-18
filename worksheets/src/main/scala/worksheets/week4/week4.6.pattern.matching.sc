/** **********   Lecture 4.6 - Pattern Matching   *************/
//quick reminder: we tried
// 1. classification (quad explosion)
// 2. type test and cast (unsafe, low-leve)
// 3. OO decomposition - didn't work for simplification

//we saw the purpose of classification and access functions in 4.5
//served the purpose of REVERSING the construction process - which ctor, what args


trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(x: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

//implement show:
def show(e: Expr): String = e match {
  case Number(x) => x.toString
  case Sum(e1, e2) => s"(${show(e1)} + ${show(e2)})"
  case Prod(e1, e2) => s"(${show(e1)} * ${show(e2)})"
  case Var(x) => x
}

show(Sum(Number(1), Number(44)))

show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))
