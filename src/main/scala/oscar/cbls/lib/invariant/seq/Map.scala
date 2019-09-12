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
import oscar.cbls.algo.seq.IntSequence
import oscar.cbls.core._

object Map {

  /**
   * @param seq a sequence of integers
   * @param mapArray an array that is taken as a function (it cannot be modified after this call)
   * @return a sequence where the value at any position p is equal to mapArray(seq(p))
   */
  def apply(seq:ChangingSeqValue,mapArray:Array[Long]):MapConstantFun = {
    new MapConstantFun(seq,(i => mapArray(i)),InvariantHelper.getMinMaxBoundsInt(mapArray)._2)
  }

  /**
   * @param seq a sequence of integers
   * @param transform a function to apply to each value occuring in the sequence (it cannot be modified after this call)
   * @return a sequence where the value at any position p is equal to transform(seq(p))
   */
  def apply(seq:ChangingSeqValue, transform:Long=>Long,maxTransform:Long) =
    new MapConstantFun(seq:ChangingSeqValue, transform:Long=>Long,maxTransform:Long)

  /**
   * @param seq a sequence of integers
   * @param mapArray an array that is taken as a function The value in this array an be variable that change value (althoug hthe content of the array cannot change after this call)
   * @return a sequence where the value at any position p is equal to mapArray(seq(p)).value
   */
  def apply(seq:ChangingSeqValue, mapArray:Array[IntValue]) =
    new MapThroughArray(seq:ChangingSeqValue, mapArray)
}


class MapConstantFun(seq:ChangingSeqValue,
          transform:Long=>Long,maxTransform:Long)
  extends SeqInvariant(seq.value.map(transform),maxTransform,
    seq.maxPivotPerValuePercent,seq.maxHistorySize)
with SeqNotificationTarget{

  setName("Map(" + seq.name + ")")

  registerStaticAndDynamicDependency(seq)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    digestUdpate(changes : SeqUpdate)
  }

  var checkpointStack = new SeqCheckpointedValueStack[IntSequence]()

  def digestUdpate(changes : SeqUpdate) {
    changes match {
      case SeqUpdateDefineCheckpoint(prev, isActive, checkpointLevel) =>
        digestUdpate(prev)

        this.releaseTopCheckpointsToLevel(checkpointLevel,true)
        defineCurrentValueAsCheckpoint(isActive)
        checkpointStack.defineCheckpoint(prev.newValue,checkpointLevel,this.newValue)

      case SeqUpdateInsert(value, position, prev) =>
        digestUdpate(prev)
        insertAtPosition(transform(value), position)

      case SeqUpdateLastNotified(seq) => ;

      case SeqUpdateMove(fromIncluded, toIncluded, after, flip, prev) =>
        digestUdpate(prev)
        move(fromIncluded, toIncluded, after, flip)

      case SeqUpdateRemove(position, prev) =>
        digestUdpate(prev)
        remove(position)

      case x@SeqUpdateRollBackToCheckpoint(checkpoint,checkpointLevel) =>

        this.releaseTopCheckpointsToLevel(checkpointLevel,false)
        rollbackToTopCheckpoint(checkpointStack.rollBackAndOutputValue(checkpoint,checkpointLevel))
        require(checkpointStack.topCheckpoint quickEquals checkpoint)


      case SeqUpdateAssign(seq) =>
        this := seq.map(transform)
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    c.check(this.value.toList equals seq.value.toList.map(transform))
  }
}

class MapThroughArray(seq:ChangingSeqValue,
                     transform:Array[IntValue])
  extends SeqInvariant(seq.value.map(v => transform(v).value),
    InvariantHelper.getMinMaxBounds(transform)._2,
    seq.maxPivotPerValuePercent,seq.maxHistorySize)
  with SeqNotificationTarget with IntNotificationTarget{

  setName("Map(" + seq.name + ")")

  registerStaticAndDynamicDependency(seq)
  registerStaticAndDynamicDependencyArrayIndex(transform)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = {
    digestUdpate(changes : SeqUpdate)
  }

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Long, NewVal: Long): Unit = {
    val impactedValue = id
    for(impactedPosition <- seq.value.positionsOfValue(impactedValue)){
      remove(impactedPosition)
      insertAtPosition(impactedPosition,NewVal)
    }
  }

  def digestUdpate(changes : SeqUpdate) {
    changes match {
      case SeqUpdateDefineCheckpoint(prev, isActive,chechpointLevel) =>
        digestUdpate(prev)
       case SeqUpdateInsert(value, position, prev) =>
        digestUdpate(prev)
        insertAtPosition(transform(value).value, position)
      case SeqUpdateLastNotified(seq) => ;
      case SeqUpdateMove(fromIncluded, toIncluded, after, flip, prev) =>
        digestUdpate(prev)
        move(fromIncluded, toIncluded, after, flip)
      case SeqUpdateRemove(position, prev) =>
        digestUdpate(prev)
        remove(position)
      case x@SeqUpdateRollBackToCheckpoint(checkpoint,chechpointLevel) =>
        digestUdpate(x.howToRollBack)
      case SeqUpdateAssign(seq) =>
        this := seq.map(v => transform(v).value)
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    c.check(this.value.toList equals seq.value.toList.map(x => transform(x).value))
  }
}

