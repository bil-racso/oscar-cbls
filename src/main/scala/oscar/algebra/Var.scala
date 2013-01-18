package oscar.algebra

/**Abstract class for variables*/
abstract class Var extends LinearExpression {

  def name: String
  val ub: Double
  val lb: Double
  val index: Int

  val cte = 0.0
  val coef = scala.collection.immutable.Map(this -> 1.0)

  override def toString = name

  override def derive(v: Var): Expression = {
    if (v == this) One
    else Zero
  }

  def *(cons: Const): LinearExpression = new CstVar(cons, this)

  override def equals(that: Any) = {
    that match {
      case other: Var => {
        other.index == index
      }
      case _ => false
    }
  }

}  