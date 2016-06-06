package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._

import scala.collection.immutable.SortedSet


object Successors {
  def apply(seq : ChangingSeqValue) : Array[CBLSSetVar] = {

    val succ: Array[CBLSSetVar] =
      Array.tabulate(seq.max - 1)(v => CBLSSetVar(seq.model, name = "next Value of" + v))

    Successors(seq, succ)

    succ
  }
}

case class Successors(sequence:ChangingSeqValue, successorValues:Array[CBLSSetVar])
  extends Invariant() with SeqNotificationTarget {

  //this one does not use checkpoint at all, so just skipping them.

  registerStaticAndDynamicDependency(sequence)
  for(i <- successorValues) i.setDefiningInvariant(this)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    performUpdate(changes:SeqUpdate)
  }

  private def performUpdate(changes:SeqUpdate){
    computeImpactedValues(changes) match{
      case None =>
      computeAllFromScratch(changes.newValue)
      case Some(values) =>

    }
  }

  def computeImpactedValues(changes:SeqUpdate):Option[SortedSet[Int]] = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //removing next val from pred

        computeImpactedValues(prev) match{
          case None => None
          case Some(impactedValue) =>
            val oldSeq = prev.newValue
            Some(impactedValue ++ oldSeq.predecessorPos2Val(pos) + value)
          }

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        computeImpactedValues(prev) match{
          case None => None
          case Some(impactedValues) =>
            val oldSeq = prev.newValue
            if(flip){
              //O(n) stuff
              Some(impactedValues
                ++ changes.newValue.predecessorPos2Val(fromIncluded)
                ++ changes.newValue.valueAtPosition(after)
                ++ oldSeq.valuesBetweenPositions(fromIncluded, toIncluded))
            }else{
              Some(impactedValues ++
                changes.newValue.predecessorPos2Val(fromIncluded) +
                toIncluded ++
                changes.newValue.valueAtPosition(after))
            }
        }

      case r@SeqUpdateRemove(position: Int, prev : SeqUpdate) =>
        computeImpactedValues(prev) match {
          case None => None
          case Some(impactedValues) =>
            val oldSeq = prev.newValue
            Some(impactedValues + r.removedValue ++ oldSeq.predecessorPos2Val(position))
        }

      case u@SeqUpdateRollBackToCheckpoint(checkpoint) =>
        computeImpactedValues(u.howToRollBack)

      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive) =>
        computeImpactedValues(prev)

      case SeqUpdateLastNotified(value) =>
        require(value quickEquals sequence.value)
        Some(SortedSet.empty[Int]) //we are starting from the previous value

      case SeqUpdateSet(value : IntSequence) =>
        None //impossible to go incremental
    }
  }

  def computeForValues(seq:IntSequence, values:SortedSet[Int]){
    for(value <- values){
      successorValues(value) := seq.positionsOfValue(value).flatMap(position => seq.successorPos2Val(position))
    }
  }

  def computeAllFromScratch(seq:IntSequence){
    val emptySet = SortedSet.empty[Int]
    successorValues.foreach(node => node := emptySet)

    var explorer = seq.explorerAtPosition(0).head
    while(explorer.next match{
      case None =>
        false
      case Some(next) =>
        successorValues(explorer.value) :+= next.value
        explorer = next
        true
    }){}
  }
}
