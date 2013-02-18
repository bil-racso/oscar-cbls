package oscar.algebra

class Frac(num: Expression, denom: Expression) extends BinaryOp {
  //require(!denom.isZero)
  val a = num
  val b = denom
  val symb = "/"
  val op = (a: Double, b: Double) => a / b
  def derive(v: Var): Expression = {
    val fprime = num.derive(v)
    val gprime = denom.derive(v)
    (fprime * denom - gprime * num) / (denom * denom)
  }
  override def isZero() = a.isZero()
}