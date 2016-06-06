package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

/**
 * the position of value a in sequence v; default if not in the sequence
 * @param v is a SeqValue
 * @param a is the value that is to locate in the sequence
 * @param defaultIfNotInSequence the value of this invariant in case a is not in the sequence.
 */
case class PositionOf(v: SeqValue, a:IntValue, defaultIfNotInSequence:Int)
  extends IntInvariant((v.value.positionOfValue(a.value) match{
    case Some(x) => x
    case None => defaultIfNotInSequence
  }), 0 to DomainHelper.safeAddMax(v.max,1))
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

  override def performInvariantPropagation(){
    this := (v.value.positionOfValue(a.value) match{
      case Some(x) => x
      case None => defaultIfNotInSequence
    })
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == (v.value.positionOfValue(a.value) match{
      case Some(x) => x
      case None => defaultIfNotInSequence
    }))
  }
}