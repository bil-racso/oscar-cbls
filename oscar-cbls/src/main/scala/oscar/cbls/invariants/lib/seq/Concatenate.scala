package oscar.cbls.invariants.lib.seq

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

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


object Concatenate {

  def apply(a : SeqValue, b : SeqValue, maxPivotPerValuePercent : Int = 4, maxHistorySize : Int = 20) : SeqValue = {
    (a,b) match {
      case (ac : CBLSSeqConst,bc : CBLSSeqConst) =>
        CBLSSeqConst(IntSequence(ac.value ++ bc.value))
      case (ac : CBLSSeqConst,bv : ChangingSeqValue) =>
        new ConcatenateFirstConstant(ac.value.toList, bv, maxPivotPerValuePercent, maxHistorySize)
      case (av : ChangingSeqValue, bc : CBLSSeqConst) =>
        new ConcatenateSecondConstant(av, bc.value.toList, maxPivotPerValuePercent, maxHistorySize)
      case (av : ChangingSeqValue, bv : ChangingSeqValue) =>
        new Concatenate(av, bv, maxPivotPerValuePercent, maxHistorySize)
    }
  }
}

class Concatenate(a:ChangingSeqValue,b:ChangingSeqValue,maxPivotPerValuePercent: Int, maxHistorySize:Int)
  extends SeqInvariant(IntSequence(a.value ++ b.value), math.max(a.max,b.max), maxPivotPerValuePercent, maxHistorySize)
  with SeqNotificationTarget {

  require(a != b)

  registerStaticAndDynamicDependency(a, 0)
  registerStaticAndDynamicDependency(b, 1)
  finishInitialization()

  var outputToRecompute:Boolean = false

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate) : Unit = {
    val isFirst = (d == 0)
    if(!outputToRecompute && !digestChanges(isFirst, changes)) {
      outputToRecompute = true
      scheduleForPropagation()
    }
  }

  override def performInvariantPropagation() : Unit = {
    if(this.outputToRecompute){
      outputToRecompute = false
      this := IntSequence(a.value ++ b.value)
    }
  }

  def digestChanges(isFirst : Boolean, changes : SeqUpdate) : Boolean = {
    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(isFirst, prev)) return false
        this.insertAtPosition(value, if (isFirst) pos else pos + a.value.size)
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(isFirst, prev)) return false
        if (isFirst) {
          this.move(fromIncluded, toIncluded, after, flip)
        } else {
          val offset = a.value.size
          this.move(fromIncluded + offset, toIncluded + offset, after + offset, flip)
        }
        true

      case SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(isFirst, prev)) return false
        this.remove(if (isFirst) position else position + a.value.size)
        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint) =>
        digestChanges(isFirst, u.howToRollBack)

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive) =>
        digestChanges(isFirst, prev)

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    c.check((a.value.toList ++ b.value.toList) equals this.value.toList,Some("a.value.toList:" + a.value.toList + " b.value.toList:" + b.value.toList + " should== this.value.toList" + this.value.toList))
  }
}


class ConcatenateFirstConstant(a:List[Int],b:ChangingSeqValue,maxPivotPerValuePercent: Int, maxHistorySize:Int)
  extends SeqInvariant(IntSequence(a ++ b.value), math.max(a.max,b.max), maxPivotPerValuePercent, maxHistorySize)
  with SeqNotificationTarget {

  registerStaticAndDynamicDependency(b)

  finishInitialization()

  var checkpoint:IntSequence = null
  var outputAtCheckpoint:IntSequence = null

  var offsetForSecond = a.size

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate) : Unit = {
    if (!digestChanges(changes)) {
      this := IntSequence(a ++ changes.newValue)
    }
  }

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.insertAtPosition(value, pos + offsetForSecond)
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.move(fromIncluded + offsetForSecond, toIncluded + offsetForSecond, after + offsetForSecond, flip)
        true

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.remove(position + offsetForSecond)
        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint) =>
        require(checkpoint quickEquals this.checkpoint)
        this.rollbackToCurrentCheckpoint(outputAtCheckpoint)
        true

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive) =>
        if(!digestChanges(prev)){
          checkpoint = prev.newValue
          outputAtCheckpoint =IntSequence(a ++ prev.newValue)
          this := outputAtCheckpoint
          this.defineCurrentValueAsCheckpoint(true)
          true
        }else{
          checkpoint = prev.newValue
          outputAtCheckpoint = this.newValue
          this.defineCurrentValueAsCheckpoint(true)
          true
        }

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    println("coucou")
    c.check((a ++ b.value.toList) equals this.value.toList,Some("a.value.toList:" + a+ " b.value.toList:" + b.value.toList + " should== this.value.toList" + this.value.toList))
  }
}


class ConcatenateSecondConstant(a:ChangingSeqValue,b:List[Int],maxPivotPerValuePercent: Int, maxHistorySize:Int)
  extends SeqInvariant(IntSequence(a.value ++ b), math.max(a.max,b.max), maxPivotPerValuePercent, maxHistorySize)
  with SeqNotificationTarget {

  registerStaticAndDynamicDependency(a)
  finishInitialization()

  var checkpoint:IntSequence = null
  var outputAtCheckpoint:IntSequence = null

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate) : Unit = {
    if (!digestChanges(changes)) {
      this := IntSequence(changes.newValue ++ b)
    }
  }

  def digestChanges(changes : SeqUpdate) : Boolean = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.insertAtPosition(value, pos)
        true

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.move(fromIncluded, toIncluded, after, flip)
        true

      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestChanges(prev)) return false
        this.remove(position)
        true

      case u@SeqUpdateRollBackToCheckpoint(checkpoint) =>
        require(checkpoint quickEquals this.checkpoint)
        this.rollbackToCurrentCheckpoint(outputAtCheckpoint)
        true

      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive) =>
        if(!digestChanges(prev)){
          checkpoint = prev.newValue
          outputAtCheckpoint = IntSequence(prev.newValue ++ b)
          this := outputAtCheckpoint
          this.defineCurrentValueAsCheckpoint(true)
          true
        }else{
          checkpoint = prev.newValue
          outputAtCheckpoint = this.newValue
          this.defineCurrentValueAsCheckpoint(true)
          true
        }

      case SeqUpdateLastNotified(value) =>
        true

      case SeqUpdateAssign(value : IntSequence) =>
        false
    }
  }

  override def checkInternals(c : Checker) : Unit = {
    println("coucou")
    c.check((a.value.toList ++ b) equals this.value.toList,Some("a.value.toList:" + a.value.toList + " b.value.toList:" + b + " should== this.value.toList" + this.value.toList))
  }
}
