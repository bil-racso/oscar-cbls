package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.seq.functional.{IntSequenceExplorer, UniqueIntSequence}
import oscar.cbls.invariants.core.computation._

import scala.collection.immutable.SortedSet

object Successor {

  def apply(seq : ChangingSeqValue,
            defaultWhenNotInSequence:Int,
            defaultWhenNoSuccessor:Int) : Array[CBLSIntVar] = {

    val succ: Array[CBLSIntVar] =
      Array.tabulate(seq.max - 1)(v => CBLSIntVar(seq.model, name = "next Value of" + v))

    Successor(seq, succ, defaultWhenNotInSequence, defaultWhenNoSuccessor)

    succ
  }
}

case class Successor(sequence:ChangingSeqValue, successorValues:Array[CBLSIntVar], defaultWhenNotInSequence:Int, defaultWhenNoSuccessor:Int)
  extends Invariant() with SeqNotificationTarget {

  registerStaticAndDynamicDependency(sequence)
  for(i <- successorValues) i.setDefiningInvariant(this)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate, willOftenRollBackToCurrentValue: Boolean) {
    computeStartValuesOfImpactedZone(changes:SeqUpdate) match{
      case None =>  computeAllFromScratch(changes.newValue)
      case Some(startUpdateValues) =>
        val startUpdateExplorersAndVal = startUpdateValues.toList.map(value => (value,changes.newValue.explorerAtValue(value)))
        val startUpdateExplorersAndValSortedByPos =
          startUpdateExplorersAndVal.sortBy(valueAndExplorer => valueAndExplorer._2 match{
            case None => -valueAndExplorer._1
            case Some(explorer) => explorer.position})

        startUpdateExplorersAndValSortedByPos.foreach(startValueAndExplorer => updateStartFrom(startValueAndExplorer._1,startValueAndExplorer._2,changes.newValue))
    }
  }

  def computeStartValuesOfImpactedZone(changes:SeqUpdate):Option[SortedSet[Int]] = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) => Some(startsOfImpactedZone + value ++ changes.newValue.predecessorVal2Val(value))
        }

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) => Some(
            startsOfImpactedZone ++
              changes.newValue.predecessorPos2Val(fromIncluded) +
              changes.newValue.valueAtPosition(fromIncluded).head +
              changes.newValue.valueAtPosition(toIncluded).head +
              changes.newValue.valueAtPosition(after).head)
        }

      case SeqUpdateRemoveValue(value : Int, prev : SeqUpdate) =>
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) => Some(
            startsOfImpactedZone ++
              changes.newValue.predecessorVal2Val(value) + value)
        }

      case SeqUpdateSet(value : UniqueIntSequence) =>
        if (value quickEquals sequence.value){
          Some(SortedSet.empty[Int]) //we are starting from the previous value
        }else{
          None //impossible to go incremental
        }
    }
  }

  def computeAllFromScratch(seq:UniqueIntSequence){
    successorValues.foreach(node => node := defaultWhenNotInSequence)

    var explorer = seq.explorerAtPosition(0).head
    while(explorer.next match{
      case None =>
        successorValues(explorer.value) := defaultWhenNoSuccessor
        false
      case Some(next) =>
        successorValues(explorer.value) := next.value
        explorer = next
        true
    }){}
  }

  def updateStartFrom(startValue:Int,startExplorer:Option[IntSequenceExplorer],seq:UniqueIntSequence){
    startExplorer match{
      case None => successorValues(startValue) := defaultWhenNotInSequence
      case Some(startExplorer) =>
        var explorer = startExplorer
        while(explorer.next match{
          case None =>
            successorValues(explorer.value) := defaultWhenNoSuccessor
            false
          case Some(next) =>
            if(successorValues(explorer.value).newValue != next.value){
              successorValues(explorer.value) := next.value
              explorer = next
              true
            }else false
        }){}
    }
  }
}