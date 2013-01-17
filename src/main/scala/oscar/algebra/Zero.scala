package oscar.algebra

case object Zero extends Const(0) {
  override def *(expr: LinearExpression) = this
  override def +(expr: LinearExpression) = expr
}