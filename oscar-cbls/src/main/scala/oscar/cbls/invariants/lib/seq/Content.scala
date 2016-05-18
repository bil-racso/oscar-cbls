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

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate, stableCheckpoint : Boolean) : Unit = {
    if(!digestUpdates(changes)) {
      this := (SortedSet.empty[Int] ++ changes.newValue.content)
      savedCheckpoint = null
      updatesFromThisCheckpointInReverseOrder = null
    }
    if(stableCheckpoint){
      savedCheckpoint = changes.newValue
      updatesFromThisCheckpointInReverseOrder = null
    }
  }

  //true if could be incremental, false otherwise
  def digestUpdates(changes : SeqUpdate):Boolean = {
    changes match{
      case SeqUpdateInsert(value:Int,pos:Int,prev:SeqUpdate) =>
        if(digestUpdates(prev)) {
          updatesFromThisCheckpointInReverseOrder = QList((value,true),updatesFromThisCheckpointInReverseOrder)
          this :+= value
          true
        }else false
      case SeqUpdateMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean,prev:SeqUpdate) =>
        digestUpdates(prev)
      case SeqUpdateRemoveValue(value:Int,prev:SeqUpdate) =>
        if(digestUpdates(prev)) {
          updatesFromThisCheckpointInReverseOrder = QList((value,false),updatesFromThisCheckpointInReverseOrder)
          this :-= value
          true
        }else false
      case SeqUpdateSet(value:UniqueIntSequence) =>
        if(value quickEquals savedCheckpoint) {
          //undo since last checkpoint
          comeBackToSavedCheckPoint()
          true
        }else if (value quickEquals v.value){
          //start at the previous value; easy game.
          true
        }else{
          false
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
