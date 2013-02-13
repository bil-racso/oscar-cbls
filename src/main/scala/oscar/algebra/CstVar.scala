package oscar.algebra

/**
 * (c * x)
 */
class CstVar(val coeff: Const, val variable: Var) extends LinearExpression {

  import scala.collection.immutable.Map
  val cte = 0.0
  val coef = if (coeff == 0) Map[Var, Double]() else Map(variable -> coeff.d)

  override def toString = coeff + "*" + variable

  override def derive(v: Var): Expression = {
    if (variable == v) coeff
    else Zero
  }

}

object CstVar {
  def unapply(l: CstVar): Option[(Const, Var)] = Some(l.coeff, l.variable)
}