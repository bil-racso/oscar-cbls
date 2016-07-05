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

import scala.collection.immutable.SortedSet


object Successors {
  def apply(seq : ChangingSeqValue) : Array[CBLSSetVar] = {

    val succ: Array[CBLSSetVar] =
      Array.tabulate(seq.max - 1)(v => CBLSSetVar(seq.model, name = "next Value of" + v))

    new Successors(seq, succ)

    succ
  }
}

class Successors(sequence:ChangingSeqValue, successorValues:Array[CBLSSetVar])
  extends Invariant() with SeqNotificationTarget {

  //this one does not use checkpoint at all, so just skipping them.

  registerStaticAndDynamicDependency(sequence)
  finishInitialization()
  for(i <- successorValues) i.setDefiningInvariant(this)


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

      case SeqUpdateAssign(value : IntSequence) =>
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
