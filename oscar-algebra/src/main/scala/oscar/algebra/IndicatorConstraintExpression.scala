package oscar.algebra

class IndicatorConstraintExpression(
  linExpr: LinearExpression,
  sense: ConstraintSense,
  val indicators: Seq[LinearExpression],
  val bigM: Option[Double]) extends LinearConstraintExpression(linExpr, sense) {

  if(indicators.length > 0)
    require(bigM.isDefined, s"M should be defined for each IndicatorConstraint")

  val constraintExpressions: Seq[LinearConstraintExpression] =
    indicators.zipWithIndex.map { case (ind, i) =>
      val actualBound = bigM.get * ind

      lazy val constraintLQ = linExpr <:= actualBound
      lazy val constraintGQ = linExpr >:= -actualBound

      sense match {
        case LQ => Seq(constraintLQ)
        case GQ => Seq(constraintGQ)
        case EQ => Seq(constraintLQ, constraintGQ)
      }
    }.reduce(_ ++ _)

  override def toString: String = constraintExpressions.mkString("\n")
}
