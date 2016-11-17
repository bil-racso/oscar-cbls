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
import oscar.cbls.invariants.core.propagation.Checker

object Map {
  def apply(seq:ChangingSeqValue,mapArray:Array[Int]):MapConstantFun = {
    new MapConstantFun(seq,mapArray,InvariantHelper.getMinMaxBoundsInt(mapArray)._2)
  }
}


class MapConstantFun(seq:ChangingSeqValue,
          transform:Int=>Int,maxTransform:Int)
  extends SeqInvariant(seq.value.map(transform),maxTransform,
    seq.maxPivotPerValuePercent,seq.maxHistorySize)
with SeqNotificationTarget{

  setName("Map(" + seq.name + ")")

  registerStaticAndDynamicDependency(seq)
  finishInitialization()

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate) : Unit = {
    digestUdpate(changes : SeqUpdate)
  }

  var checkpointStack:List[(IntSequence,IntSequence)] = List.empty
  var currentCheckpointLevel = -1

  def myReleaseCheckpointToAndIncluding(level:Int){
    while(currentCheckpointLevel >= level){
      checkpointStack = checkpointStack.tail
      currentCheckpointLevel -=1
      releaseTopCheckpoint()
    }
  }


  def digestUdpate(changes : SeqUpdate) {
    changes match {
      case SeqUpdateDefineCheckpoint(prev, isActive, checkpointLevel) =>
        digestUdpate(prev)

        myReleaseCheckpointToAndIncluding(checkpointLevel)

        defineCurrentValueAsCheckpoint(isActive)
        checkpointStack = (prev.newValue,this.newValue) :: checkpointStack

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

        myReleaseCheckpointToAndIncluding(checkpointLevel+1)

        require(checkpointStack.head._1 quickEquals checkpoint)
        rollbackToTopCheckpoint(this.checkpointStack.head._2)

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

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate) : Unit = {
    digestUdpate(changes : SeqUpdate)
  }

  override def notifyIntChanged(v : ChangingIntValue, id : Int, OldVal : Int, NewVal : Int) : Unit = {
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

