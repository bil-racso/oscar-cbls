package oscar.algebra

class Sin(expr: Expression) extends UnaryOp(expr: Expression, "cos", math.sin _) {
  def derive(v: Var): Expression = {
    expr.derive(v) * cos(expr)
  }
}