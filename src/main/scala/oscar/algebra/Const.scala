package oscar.algebra

class Const(val d: Double) extends LinearExpression {

  val cte = d
  val coef = scala.collection.immutable.Map[Var, Double]()

  def *(expr: LinearExpression): LinearExpression = new LinearExpressionProd(this, expr)

  def *(c2: Const) = new Const(d * c2.d)

  def +(c2: Const) = new Const(d + c2.d)

  def -(c2: Const) = new Const(d - c2.d)

  def *(x: Var) = new CstVar(this, x)

  override def toString = d.toString

  override def derive(v: Var): Expression = Zero

}

object Const {
  def apply(d: Double): Const = d match {
    case 0.0 => Zero
    case 1.0 => One
    case _ => new Const(d)
  }
  def unapply(c: Const): Option[Double] = Some(c.d)
}  