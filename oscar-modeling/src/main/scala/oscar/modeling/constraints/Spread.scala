package oscar.modeling.constraints

import oscar.modeling.algebra.integer.IntExpression

/**
  * Created by dervalguillaume on 8/06/16.
  */
case class Spread(x: Iterable[IntExpression], s: Int, s2: IntExpression) extends Constraint