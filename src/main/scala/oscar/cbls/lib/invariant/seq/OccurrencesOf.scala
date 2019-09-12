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

/**
 * the number of occurrences of value a in sequence v; default if not in the sequence
 * @param v is a SeqValue
 * @param a is the value that is to locate in the sequence
 */
case class OccurrencesOf(v: SeqValue, a:IntValue)
  extends IntInvariant(v.value.nbOccurrence(a.value), Domain(0L , Int.MaxValue))
  with SeqNotificationTarget with IntNotificationTarget{

  setName("OccurrencesOf(" + a.name + " in " + v.name + ")")

  registerStaticAndDynamicDependency(v)
  registerStaticAndDynamicDependency(a)

  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    scheduleForPropagation()
  }

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long) {
    scheduleForPropagation()
  }

  override def performInvariantPropagation() {
    this := v.value.nbOccurrence(a.value)
  }

  override def checkInternals(c: Checker) {
    require(this.value == v.value.positionsOfValue(a.value).size, "this.value:" + this.value + " v.value.positionsOfValue(a.value).size:" + v.value.positionsOfValue(a.value).size + " v.value.nbOccurrence(a.value):" + v.value.nbOccurrence(a.value))
  }
}

