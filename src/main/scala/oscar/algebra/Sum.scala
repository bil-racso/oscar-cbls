package oscar.algebra

class Sum(val a: Expression, val b: Expression) extends BinaryOp {
  val symb = "+"
  val op = (a: Double, b: Double) => a + b
  def derive(v: Var): Expression = {
    a.derive(v) + b.derive(v)
  }
  override def isZero() = a.isZero() && b.isZero()
}