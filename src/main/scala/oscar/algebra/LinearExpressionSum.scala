package oscar.algebra

/**
 * (linExpr1 + linExpr2)
 */
class LinearExpressionSum(expr1: LinearExpression, expr2: LinearExpression) extends LinearExpressionBinary(expr1, expr2) {

  val opStr = "+"

  def op(v1: Double, v2: Double) = v1 + v2

  override def derive(v: Var): Expression = {
    expr1.derive(v) + expr2.derive(v)
  }
}