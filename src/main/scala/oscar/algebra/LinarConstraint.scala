package oscar.algebra

/**
 * A linear constraint has the form (linearExpression REL 0) with REL in {<=, ==, >=}
 */
class LinearConstraint(val linExpr: LinearExpression, val consType: ConstraintType.Value) {
  override def toString = linExpr + " " + consType + " " + 0
}