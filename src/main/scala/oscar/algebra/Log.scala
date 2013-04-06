package oscar.algebra

class Log(expr: Expression) extends UnaryOp(expr: Expression, "log", math.log _) {
  def derive(v: Var): Expression = {
    expr.derive(v) / expr
  }
  override def isZero() = expr match {
    case Const(1) => true
    case _ => false
  }
}