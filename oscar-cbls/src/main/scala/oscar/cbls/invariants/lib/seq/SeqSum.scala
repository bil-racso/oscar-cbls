package oscar.cbls.invariants.lib.seq

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

import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{ErrorChecker, Checker}


/**
 * sum(v) (the sum of all element in v; if a value has multiple occurrences, they are all summed)
 * @param v is a SeqValue, containing a number of values, to sum
 * @author renaud.delandtsheer@cetic.be
 */
case class SeqSum(v: SeqValue)
  extends IntInvariant()
  with SeqNotificationTarget{

  setName("SeqSum(" + v.name + ")")

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  this := computeSumFromScratch(v.value)
  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    checkInternals(new ErrorChecker())
    if (!digestChanges(changes)) {
      this := computeSumFromScratch(changes.newValue)
    }
  }

  private def computeSumFromScratch(v:IntSequence):Int = {
    var contentWithOccurences = v.unorderedContentNoDuplicateWithNBOccurences
    var sum = 0
    while(contentWithOccurences match{
      case Nil => return sum
      case (value,occ) :: tail =>
        sum += (value*occ)
        contentWithOccurences = tail
        true
    }){}
    0
  }

  var checkpoint:IntSequence = null
  var outputAtCheckpoint:Int = -1

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this :+= value
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
         digestChanges(prev) //nothing to do :-)

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this :-= r.removedValue
        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint) =>
        require(checkpoint quickEquals this.checkpoint)
        this := outputAtCheckpoint
        true

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive) =>
        if(!digestChanges(prev)){
          checkpoint = prev.newValue
          outputAtCheckpoint = computeSumFromScratch(changes.newValue)
          this := outputAtCheckpoint
          true
        }else{
          checkpoint = prev.newValue
          outputAtCheckpoint = this.newValue
          true
        }

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c: Checker) {
    c.check(this.newValue == computeSumFromScratch(v.value),Some("this.newValue(="+ this.newValue+") == Sum(v.value(="+ computeSumFromScratch(v.value)+ ")"))
  }
}

