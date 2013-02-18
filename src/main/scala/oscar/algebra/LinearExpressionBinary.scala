package oscar.algebra

/**
 * (expr1 + expr2) or (expr1 - expr2)
 */
abstract class LinearExpressionBinary(expr1: LinearExpression, expr2: LinearExpression) extends LinearExpression {

  val cte = op(expr1.cte, expr2.cte)
  val coef = merge()

  def op(v1: Double, v2: Double): Double

  val opStr: String

  override def toString() = "(" + expr1 + opStr + expr2 + ")"

  def merge(): scala.collection.immutable.Map[Var, Double] = {
    import scala.collection.mutable.Map
    val mymap = Map[Var, Double]()
    for ((k, v) <- expr1.coef) {
      mymap += k -> v
    }
    for ((k, v) <- expr2.coef) {
      mymap.get(k) match {
        case Some(c) => mymap(k) = op(c, v)
        case None => mymap += (k -> op(0, v))
      }
    }
    import scala.collection.immutable.Map
    mymap.filterNot(_._2 == 0).toMap
  }

}  