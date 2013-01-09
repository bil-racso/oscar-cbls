package oscar.algebra

class Prod(val a: Expression, val b: Expression) extends BinaryOp {
  val symb = "*"
  val op = (a: Double, b: Double) => a * b
  def derive(v: Var): Expression = {
    a * b.derive(v) + b * a.derive(v)
  }
  override def toString = "(" + a + ")*(" + b + ")"
  override def isZero() = a.isZero() || b.isZero()
}