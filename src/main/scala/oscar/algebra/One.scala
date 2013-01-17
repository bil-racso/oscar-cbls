package oscar.algebra

case object One extends Const(1) {
  override def *(expr: LinearExpression) = expr
}  