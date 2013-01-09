package oscar.algebra

/**Abstract type for variables*/

/**
 * @author Pierre Schaus pschaus@gmail.com 
 */
abstract class LinearExpression extends Expression {

  val cte: Double
  val coef: scala.collection.immutable.Map[Var, Double]

  def +(expr: LinearExpression): LinearExpression = new LinearExpressionSum(expr, this)

  def -(expr: LinearExpression): LinearExpression = new LinearExpressionDiff(this, expr)

  def unary_- : LinearExpression = new LinearExpressionDiff(0, this)

  def <=(linExpr: LinearExpression) = new LinearConstraint(this - linExpr, ConstraintType.LQ)

  def >=(linExpr: LinearExpression) = new LinearConstraint(this - linExpr, ConstraintType.GQ)

  def ==(linExpr: LinearExpression) = new LinearConstraint(this - linExpr, ConstraintType.EQ)

  /**
   * Test if two linear expressions are logically equivalent
   */
  override def equals(that: Any) = {
    that match {
      case other: LinearExpression => {
        other.cte == cte && other.coef == coef
      }
      case _ => false
    }
  }

  def eval(env: Var => Double): Double = cte + coef.map(e => env(e._1) * e._2).sum

  override def value: Option[Double] = {
    var res = cte
    for ((x, a) <- coef) {
      x.value match {
        case None => return None
        case Some(v) => {
          res += a * v
        }
      }
    }
    Some(res)
  }

  override def derive(x: Var): Expression = {
    coef.get(x) match {
      case None => Zero
      case Some(v: Double) => new Const(v)
    }
  }
}

