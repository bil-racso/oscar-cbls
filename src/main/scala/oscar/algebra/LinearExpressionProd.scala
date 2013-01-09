package oscar.algebra

/**
 * (c * linExpr)
 */
class LinearExpressionProd(val c: Const, val expr: LinearExpression) extends LinearExpression {
  import scala.collection.immutable.Map
  val cte = if (c == Zero) 0.0 else c.d * expr.cte
  val coef = if (c == Zero) Map[Var, Double]() else expr.coef.map(e => (e._1 -> c.d * e._2))

  override def derive(v: Var): Expression = {
    c * expr.derive(v)
  }
  override def toString = c + "*(" + expr + ")"

} 