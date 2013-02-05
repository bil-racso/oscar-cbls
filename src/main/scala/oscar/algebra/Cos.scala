package oscar.algebra

class Cos(expr: Expression) extends UnaryOp(expr: Expression, "cos", math.cos _) {
  def derive(v: Var): Expression = {
    Zero - expr.derive(v) * sin(expr)
  }
}