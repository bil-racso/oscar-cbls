package oscar.cbls.invariants.lib.seq.draft

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation._



/*

case class RoutePredecessor(s:ChangingSeqValue, v:Int, values:Array[Int], predecessorValues:Array[CBLSIntVar], defaultPosition:Int)
  extends Invariant() with SeqNotificationTarget with IntNotificationTarget{

  registerStaticAndDynamicDependency(s)
  registerStaticAndDynamicDependencyArrayIndex(values)
  finishInitialization()

  val savedValues:Array[Int] = computeValueFromScratch(routes.value)
  var savedCheckpoint = routes.value
  val touchedRoutesSinceCheckpointArray:Array[Boolean] = Array.fill(v)(false)
  var touchedRoutesSinceCheckpointList:QList[Int] = null

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean){
    if(!digestUpdates(changes)) {
      affect(savedValues)
      savedCheckpoint = null
    }
    if(stableCheckpoint){
      saveCurrentCheckpoint(changes.newValue)
    }
  }

  def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqInsert(value : Int, pos : Int, prev : SeqUpdate) =>

      case SeqMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>

      case SeqRemoveValue(value : Int, prev : SeqUpdate) =>

      case Set(value : UniqueIntSequence) =>
    }
  }

  def computeFromScratch(){
    val seq = s.value
    for(i <- values.indices){
      positions(i) := (seq.positionOfValue(values(i).value) match{case None => defaultPosition case Some(x) => x})
    }
  }
}

case class ElementsBetween(s:ChangingSeqValue, values:Array[Int], positions:Array[CBLSIntVar])
/*  extends Invariant() with SeqNotificationTarget{

  registerStaticAndDynamicDependency(s)
  finishInitialization()

  val savedValues:Array[Int] = computeValueFromScratch(routes.value)
  var savedCheckpoint = routes.value
  val touchedRoutesSinceCheckpointArray:Array[Boolean] = Array.fill(v)(false)
  var touchedRoutesSinceCheckpointList:QList[Int] = null

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean){
    if(!digestUpdates(changes)) {
      affect(savedValues)
      savedCheckpoint = null
    }
    if(stableCheckpoint){
      saveCurrentCheckpoint(changes.newValue)
    }
  }

  def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqInsert(value : Int, pos : Int, prev : SeqUpdate) =>

      case SeqMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>

      case SeqRemoveValue(value : Int, prev : SeqUpdate) =>

      case Set(value : UniqueIntSequence) =>
}*/

*/