package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation._

import scala.collection.immutable.SortedSet


//one more partition for values that are not present, but belong to PartitionOf
/*
/**
 * respects the route conditions: you cannot cut on parttions
 * @param s
 * @param pivot
 * @param partitions
 * @param partitionOf
 */
case class RoutePartition(s:ChangingSeqValue, v:Int, nodesOfVehicle:Array[CBLSSetVar], vehicleOfNode:Array[CBLSIntVar])
  extends Invariant() with SeqNotificationTarget{

  val nbPivot = pivot.length
  require(partitions.length == pivot.length  + 1)

  registerStaticAndDynamicDependency(s)
  for(p <- partitions) p.setDefiningInvariant(this)
  for(p <- partitionOf) p.setDefiningInvariant(this)
  finishInitialization()

  def computePartitionsFromScratch(seq:UniqueIntSequence){
    val partitions = Array.tabulate(nbPivot)

  }

  def affect(partitions:Array[SortedSet[Int]],partitionOf:Array[Int]){
    for(i <- partitions.indices){
      this.partitions(i) := partitions(i)
    }
    for(j <- partitionOf.indices){
      this.partitionOf(j) := partitionOf(j)
    }
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean){
    if(!digestUpdates(changes)) {
      affect(computePartitionsFromScratch(changes.newValue))
    }
    if(stableCheckpoint){
      saveCurrentCheckpoint(changes.newValue)
    }
  }

  def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        false
      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        false
      case SeqUpdateRemoveValue(value : Int, prev : SeqUpdate) =>
        false
      case SeqUpdateSet(value : UniqueIntSequence) =>
        false
    }
  }
}
*/