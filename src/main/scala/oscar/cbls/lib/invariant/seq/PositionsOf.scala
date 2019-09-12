package oscar.cbls.lib.invariant.seq

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import oscar.cbls._
import oscar.cbls.core._

object PositionsOf{

  /**
   * Maintains the position of value of variable a in the sequence v.
   * @param v a sequence
   * @param a an intValue, which can be a CBLSIntVar for instance
   * @return a ChangingSetValue that is maintained as the set of position in v where the value is the one of a
   */
  def apply(v: SeqValue, a:IntValue):ChangingSetValue =
    new PositionsOf(v, a)

  /**
   * Maintains the position of value of variable a in the sequence v.
   * @param v a sequence
   * @param a an integer
   * @return a ChangingSetValue that is maintained as the set of position in v where the value is a
   */
  def apply(v: SeqValue, a:Int):ChangingSetValue  =
    new PositionsOfConst(v, a)
}

/**
 * the position of value a in sequence v; default if not in the sequence
 * @param v is a SeqValue
 * @param a is the value that is to locate in the sequence
 */
class PositionsOf(v: SeqValue, a:IntValue)
  extends SetInvariant(v.value.positionsOfValueSet(a.value), 0 to DomainHelper.safeAddMax(v.max,1))
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
    this := v.value.positionsOfValueSet(a.value)
  }

  override def checkInternals(c: Checker) {
    c.check(this.value equals v.value.positionsOfValue(a.value))
  }
}

/**
 * the position of value a in sequence v; default if not in the sequence
 * @param v is a SeqValue
 * @param a is the value that is to locate in the sequence
 */
class PositionsOfConst(v: SeqValue, a:Int)
  extends SetInvariant(v.value.positionsOfValueSet(a), 0 to DomainHelper.safeAddMax(v.max,1))
  with SeqNotificationTarget{

  setName("PositionOf(" + a + " in " + v.name + ")")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    scheduleForPropagation()
  }

  override def performInvariantPropagation() {
    this := v.value.positionsOfValueSet(a)
  }

  override def checkInternals(c: Checker) {
    c.check(this.value equals v.value.positionsOfValueSet(a))
  }
}

