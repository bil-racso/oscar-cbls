package oscar.algebra

class Tan(expr: Expression) extends UnaryOp(expr: Expression, "tan", math.tan _) {
  def derive(v: Var): Expression = {
    expr.derive(v) / (cos(expr) * cos(expr))
  }
}