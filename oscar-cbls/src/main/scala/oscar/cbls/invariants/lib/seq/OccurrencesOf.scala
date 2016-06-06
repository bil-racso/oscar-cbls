package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

/**
 * the position of value a in sequence v; default if not in the sequence
 * @param v is a SeqValue
 * @param a is the value that is to locate in the sequence
 */
case class OccurrencesOf(v: SeqValue, a:IntValue)
  extends IntInvariant(v.value.nbOccurrence(a.value), 0 to DomainHelper.safeAddMax(v.max,1))
  with SeqNotificationTarget with IntNotificationTarget{

  setName("PositionOf(" + a.name + " in " + v.name + ")")

  registerStaticAndDynamicDependency(v)
  registerStaticAndDynamicDependency(a)

  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    scheduleForPropagation()
  }

  override def notifyIntChanged(v : ChangingIntValue, id : Int, OldVal : Int, NewVal : Int){
    scheduleForPropagation()
  }

  override def performInvariantPropagation() {
    this := v.value.nbOccurrence(a.value)
  }

  override def checkInternals(c: Checker) {
    c.check(this.value equals v.value.positionsOfValue(a.value))
  }
}

