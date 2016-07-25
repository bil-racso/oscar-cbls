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

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * content of v
 * @param v is a SeqValue, the values appearing in the sequence
 * @author renaud.delandtsheer@cetic.be
 */
case class Content(v:SeqValue)
  extends SetInvariant(SortedSet.empty[Int] ++ v.value.unorderedContentNoDuplicate,v.domain)
  with SeqNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  //TODO: handle inactive checkpoints as well.
  //note sure that such checkpoint actually help...
  var savedCheckpoint:IntSequence = v.value
  var updatesFromThisCheckpointInReverseOrder:QList[(Int,Boolean)]=null

  private def saveNewCheckpoint(u:IntSequence){
    savedCheckpoint = u
    updatesFromThisCheckpointInReverseOrder = null
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) : Unit = {

    if(!digestUpdates(changes,false)) {
      updateFromScratch(changes.newValue)
      savedCheckpoint = null
      updatesFromThisCheckpointInReverseOrder = null
    }
  }

  private def updateFromScratch(u:IntSequence){
    this := (SortedSet.empty[Int] ++ u.unorderedContentNoDuplicate)
  }

  //true if could be incremental, false otherwise
  def digestUpdates(changes : SeqUpdate, skipNewCheckpoints:Boolean):Boolean = {
    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if (!digestUpdates(prev, skipNewCheckpoints)) return false
        updatesFromThisCheckpointInReverseOrder = QList((value, true), updatesFromThisCheckpointInReverseOrder)
        this :+= value
        true
      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestUpdates(prev, skipNewCheckpoints)
      case r@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        if (!digestUpdates(prev, skipNewCheckpoints)) return false
        val value = r.removedValue
        if (changes.newValue.nbOccurrence(value) == 0){
          this :-= value
          updatesFromThisCheckpointInReverseOrder = QList((value, false), updatesFromThisCheckpointInReverseOrder)
        }
        true
      case SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) =>
        require(checkpoint quickEquals savedCheckpoint)
        comeBackToSavedCheckPoint()
        true
      case SeqUpdateLastNotified(value) =>
        require(value quickEquals v.value)
        //start at the previous value; easy game.
        true
      case SeqUpdateSet(value:IntSequence) =>
        //raw assign, no incremental possible
        false
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive:Boolean) =>
        if(skipNewCheckpoints || !isActive) {
          digestUpdates(prev,true)
        } else {
          if(!digestUpdates(prev,true)){
            //update was not incremental, do the computation now
            updateFromScratch(prev.newValue)
          }
          saveNewCheckpoint(prev.newValue)
          true
        }
    }
  }

  private def comeBackToSavedCheckPoint(){
    while(updatesFromThisCheckpointInReverseOrder!= null){
      val (valueToUndo,wasInserted) = updatesFromThisCheckpointInReverseOrder.head
      updatesFromThisCheckpointInReverseOrder = updatesFromThisCheckpointInReverseOrder.tail
      if(wasInserted){
        this :-= valueToUndo
      }else{
        this :+= valueToUndo
      }
    }
  }

  override def checkInternals(c: Checker) {
    require(v.value quickEquals latestVal)
    c.check(this.newValue.toList.sorted equals v.value.unorderedContentNoDuplicate.sorted,
      Some("this.value:" + this.value + " == v.value.content:" + v.value.unorderedContentNoDuplicate.sorted + " too much in output:" + this.value.diff(SortedSet.empty[Int] ++ v.value.unorderedContentNoDuplicate.sorted) + " missing in output:" + (SortedSet.empty[Int] ++ v.value.unorderedContentNoDuplicate.sorted).diff(this.value)+ " v:" + v))
  }
}
