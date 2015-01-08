package oscar.cbls.constraints.lib.basic

import oscar.cbls.invariants.core.computation._
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.core.propagation.Checker

/**
 * implements v \in set
 * @author renaud.delandtsheer@cetic.be
 */
case class BelongsTo(v: IntValue, set: SetValue) extends Constraint with Invariant {
  registerConstrainedVariables(v, set)
  registerStaticAndDynamicDependenciesNoID(v, set)
  finishInitialization()

  val Violation: CBLSIntVar = CBLSIntVar(model, 0 to 1, (if (set.value.contains(v.value)) 0 else 1), "belongsTo(" + v.name + "," + set.name + ")")

  Violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    Violation := (if (set.value.contains(v.value)) 0 else 1)
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    if (this.v.value == value) Violation := 0
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    if (this.v.value == value) Violation := 1
  }

  /** the violation is 1 if v is not in set, 0 otherwise*/
  override def violation = Violation
  /** the violation is 1 v is not is set, 0 otherwise*/
  override def violation(v: AbstractVariable): IntValue = { if (this.v == v || this.set == v) Violation else 0 }

  /**
   * To override whenever possible to spot errors in invariants.
   * this will be called for each invariant after propagation is performed.
   * It requires that the Model is instantiated with the variable debug set to true.
   */
  override def checkInternals(c: Checker) {
    c.check(Violation.value == (if (set.value.contains(v.value)) 0 else 1),
      Some("Violation.value (" + Violation.value
        + ") == (if(set.value" + set.value + ".contains(v.value (" + v.value + "))) 0 else 1)"))
  }
}
