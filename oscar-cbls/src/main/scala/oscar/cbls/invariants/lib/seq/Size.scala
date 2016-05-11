package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

/**
 * #(v) (cardinality, or length (since a SeqValue can only contain at most one instance of any int value)
 * @param v is a SeqValue, containing a number of values, to count
 * @author renaud.delandtsheer@cetic.be
 */
case class Size(v: SeqValue)
  extends IntInvariant(v.value.size, 0 to (v.max - v.min +1))
  with SeqNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean){
    this := changes.newValue.size
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == v.value.size, Some("this.value == v.value.size"))
  }
}