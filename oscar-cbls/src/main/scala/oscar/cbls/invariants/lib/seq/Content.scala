package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * content of v
 * @param v is a SeqValue, the values appearing in the sequence
 * @author renaud.delandtsheer@cetic.be
 */
case class Content(v:SeqValue)
  extends SetInvariant(SortedSet.empty[Int] ++ v.value.content,v.domain)
  with SeqNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  var savedCheckpoint:UniqueIntSequence = v.value
  var updatesFromThisCheckpointInReverseOrder:QList[(Int,Boolean)]=null

  private def saveNewCheckpoint(u:UniqueIntSequence){
    savedCheckpoint = u
    updatesFromThisCheckpointInReverseOrder = null
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) : Unit = {
    println("content notified " + changes)
    if(!digestUpdates(changes,false)) {
      updateFromScratch(changes.newValue)
      savedCheckpoint = null
      updatesFromThisCheckpointInReverseOrder = null
    }
  }

  private def updateFromScratch(u:UniqueIntSequence){
    this := (SortedSet.empty[Int] ++ u.content)
  }

  //true if could be incremental, false otherwise
  def digestUpdates(changes : SeqUpdate, skipNewCheckpoints:Boolean):Boolean = {
    changes match{
      case SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
        if(digestUpdates(prev,skipNewCheckpoints)) {
          updatesFromThisCheckpointInReverseOrder = QList((value,true),updatesFromThisCheckpointInReverseOrder)
          this :+= value
          true
        }else false
      case SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
        digestUpdates(prev,skipNewCheckpoints)
      case SeqUpdateRemoveValue(value:Int,prev:SeqUpdate) =>
        if(digestUpdates(prev,skipNewCheckpoints)) {
          updatesFromThisCheckpointInReverseOrder = QList((value,false),updatesFromThisCheckpointInReverseOrder)
          this :-= value
          true
        }else false
      case SeqUpdateRollBackToCheckpoint(checkpoint:UniqueIntSequence) =>
        require(checkpoint quickEquals savedCheckpoint)
        comeBackToSavedCheckPoint()
        true
      case SeqUpdateLastNotified(value) =>
        require(value quickEquals v.value)
        //start at the previous value; easy game.
        true
      case SeqUpdateSet(value:UniqueIntSequence) =>
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
    c.check(this.value.toList.sorted equals v.value.content.toList.sorted, Some("this.value == v.value.content"))
  }
}
