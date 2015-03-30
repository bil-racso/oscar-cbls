package oscar.nogood.core

import oscar.nogood.decisions.Decision
import oscar.cp.core.Constraint
import oscar.cp.or

class Nogood(val decisions: Array[Decision]) {
  
  def isEntailed: Boolean = decisions.forall(_.isTrue)
  
  def toConstraint: Constraint = {
    if (decisions.isEmpty) new Unfeasible(null)
    or(decisions.map(_.toLiteral))
  }
  
  def size = decisions.length
  
  override def toString: String = decisions.mkString(", ")
}