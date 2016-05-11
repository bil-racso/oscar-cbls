package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * #(v) (cardinality)
 * @param v is an IntSetVar, the set of integers to count
 * @author renaud.delandtsheer@cetic.be
 */
case class Cardinality(v: SeqValue)
  extends IntInvariant(v.value.size, 0 to (v.max - v.min +1))
  with SeqNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean){
    this := changes.newValue.size
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == v.value.size, Some("this.value == v.value.size"))
  }
}

/*
//tout ce qui vient après
case class Partition(v:SeqValue,pivots:Array[Int])
  extends Invariant
  with SeqNotificationTarget{

  val pivotValues:SortedMap[Int,Int] = SortedMap.empty ++ pivots.zipWithIndex

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  def computePartition(u:UniqueIntSequence):Array[SortedSet[Int]] = {
    val toReturn:Array[SortedSet[Int]] = Array.fill(pivots.length)(SortedSet.empty)
    var nextPivotID = 0 //next value before switching
    for(i <- u){
      if(i == pivots(nextPivotID)){
        //switching pivot
        nextPivotID += 1
        toReturn(nextPivotID) += i
      }else{ //no switching pivot
        if(nextPivotID >=0) toReturn(nextPivotID) += i
      }
    }
    toReturn
  }

  def loadPartition(partition:Array[SortedSet[Int]]){
    for((set,id) <- partition.zipWithIndex){
      partitions(id) := set
    }
  }

  val partitions = {
    val initialValues = computePartition(v.value)
    Array.tabulate(pivots.length)((id:Int) =>
      new CBLSSetVar(this.model, initialValues(id), v.domain, "partitionStartingAt" + pivots(id)))
  }

  val positionOfPivots:Array[Option[Int]] = pivots.map(pivot => v.value.positionOfValue(pivot))

  var savedCheckpoint:UniqueIntSequence = null
  var setsAtThisCheckpoint:Array[SortedSet[Int]] = null
  var currentOutputSeq:UniqueIntSequence = v.value

  def reloadSets(seq:UniqueIntSequence):Boolean = {
    if(seq != savedCheckpoint) return false
    loadPartition(setsAtThisCheckpoint)
    true
  }

  def saveCurrentCheckpoint(seq:UniqueIntSequence){
    savedCheckpoint = seq
    setsAtThisCheckpoint = partitions.map(_.newValue)
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean){
    if(!digestChanges(changes)) loadPartition(computePartition(changes.valueAfterThisUpdate))
    if(stableCheckpoint) saveCurrentOutput(changes.valueAfterThisUpdate)
  }

  //true if incremental, false otherwise and in this case, go for non-incremental
  private def digestChanges(u:SeqUpdate):Boolean = {
    u match{
      case SetORRestore(s) => reloadSets(s)
      case SeqInsert(value:Int,pos:Int,prev) => if(digestChanges(prev)){
        //it has been performed, and we must go incremental
        //search for startng pivot
        pivotValues.get(value) match {
          case Some(id) =>
          //this is a pivot, actually, and id is its ID
            //So we must partition the pivot below
          case None =>
            //this is not a pivot, let's look for a pivot
            pivots.indices.minBy(pivotId => if (pivotValues(pivotId) <= value) value - pivotValues(pivotId) <=)
        }
        }else{
        //it has not been digested, no incremental update is possible.
          for(id <- pivots.length){
            if()
          }
        }
      case SeqMove(fromIncluded:Int,toIncluded:Int,after:Int,flip:Boolean) =>
        val (smallest,largest) = InvariantHelper.getMinMaxBoundsInt(List(fromIncluded,toIncluded,after))
        val largestPivotIDBelow = pivots.indices.maxBy(pivotId => if (pivotValues(pivotId) <= smallest) pivotValues(pivotId) else Int.MinValue)
        if(u.positionOfValueNew(pivots(largestPivotIDBelow)))
      case SeqRemoveValue(value:Int) =>
    }

  }
}
*/