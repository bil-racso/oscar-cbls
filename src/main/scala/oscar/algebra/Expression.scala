package oscar.algebra

abstract class Expression {

  def value: Option[Double]

  def eval(env: Var => Double): Double

  def +(expr: Expression): Expression = new Sum(this, expr)

  def -(expr: Expression): Expression = new Diff(this, expr)

  def *(expr: Expression): Expression = new Prod(this, expr)

  def /(expr: Expression): Expression = new Frac(this, expr)

  def derive(x: Var): Expression

  def isZero(): Boolean = false
}
  