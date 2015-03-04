package oscar.cbls.constraints.lib.basic

import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.computation.{Variable, CBLSSetVar, CBLSIntVar}
import oscar.cbls.invariants.lib.set.Cardinality

/**
 * implements \exists i, i \in set
 * @author gael.thouvenin@student.umons.ac.be
 */
case class Empty(set: CBLSSetVar) extends Constraint {
  private val content = EQ(Cardinality(set), 0)

  override def violation(v: Variable): CBLSIntVar = content.violation(v)

  override def violation: CBLSIntVar = content.violation
}
