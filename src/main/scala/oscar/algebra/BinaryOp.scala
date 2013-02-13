package oscar.algebra

abstract class BinaryOp extends Expression {
  val a: Expression
  val b: Expression
  val symb: String
  val op: (Double, Double) => Double
  override def toString = "(" + a + symb + b + ")"

  def eval(env: Var => Double) = {
    op(a.eval(env), b.eval(env))

  }

  def value = {
    (a.value, b.value) match {
      case (Some(va), Some(vb)) => Some(op(va, vb))
      case _ => None
    }
  }
}