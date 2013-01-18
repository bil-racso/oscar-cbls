package oscar.algebra

//Unary operators
abstract class UnaryOp(expr: Expression, name: String, f: Double => Double) extends Expression {

  override def toString = name + "(" + expr + ")"

  def value = expr.value match {
    case Some(v) => Some(f(v))
    case None => None
  }

  def eval(env: Var => Double) = f(expr.eval(env))
}