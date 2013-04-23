package oscar.cp.scheduling2.constraints

import oscar.cp.core.Constraint

abstract class CumulativeConstraint extends Constraint(null, "Cumulative Constraint") {
  
  /** true if all the activities are accepted by the constraint **/
  private def checkActivities(): Boolean = false
  
  
}