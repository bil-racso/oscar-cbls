package oscar.cbls.invariants.lib.seq

/*
import oscar.cbls.invariants.core.algo.seq.functional.{UniqueIntSequenceExplorer, UniqueIntSequence}
import oscar.cbls.invariants.core.computation._

import scala.collection.immutable.SortedSet

case class RouteSuccessor(routes:ChangingSeqValue, v:Int, successorValues:Array[CBLSIntVar], defaultWhenNotInSequence:Int)
  extends Invariant() with SeqNotificationTarget {

  registerStaticAndDynamicDependency(routes)
  for(i <- successorValues) i.setDefiningInvariant(this)
  finishInitialization()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
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
          case Some(startsOfImpactedZone) => Some(startsOfImpactedZone + value + RoutingConventionMethods.routingPredVal2Val(value,changes.newValue,v))
        }

      case SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) => Some(
            startsOfImpactedZone +
              RoutingConventionMethods.routingPredPos2Val(fromIncluded,changes.newValue,v) +
              changes.newValue.valueAtPosition(fromIncluded).head +
              changes.newValue.valueAtPosition(toIncluded).head +
              changes.newValue.valueAtPosition(after).head)
        }

      case SeqUpdateRemoveValue(value : Int, prev : SeqUpdate) =>
        computeStartValuesOfImpactedZone(prev) match{
          case None => None
          case Some(startsOfImpactedZone) => Some(
            startsOfImpactedZone +
              RoutingConventionMethods.routingPredPos2Val(value,changes.newValue,v) + value)
        }

      case SeqUpdateLastNotified(value) =>
        require (value quickEquals routes.value)
        Some(SortedSet.empty[Int]) //we are starting from the previous value
      case SeqUpdateSet(value : UniqueIntSequence) =>
        None //impossible to go incremental
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,_) =>
        computeStartValuesOfImpactedZone(prev)
      case u@SeqUpdateRollBackToCheckpoint(checkpoint:UniqueIntSequence) =>
        computeStartValuesOfImpactedZone(u.howToRollBack)
    }
  }

  def computeAllFromScratch(seq:UniqueIntSequence){
    successorValues.foreach(node => node := defaultWhenNotInSequence)

    var explorer = seq.explorerAtPosition(0).head
    while(explorer.next match{
      case None =>
        successorValues(explorer.value) := v-1
        false
      case Some(next) =>
        if(next.value < v){
          successorValues(explorer.value) := next.value - 1
        }else{
          successorValues(explorer.value) := next.value
        }
        explorer = next
        true
    }){}
  }

  def updateStartFrom(startValue:Int,startExplorer:Option[UniqueIntSequenceExplorer],seq:UniqueIntSequence){
    startExplorer match{
      case None => successorValues(startValue) := defaultWhenNotInSequence
      case Some(startExplorer) =>
        var explorer = startExplorer
        while(explorer.next match{
          case None =>
            successorValues(explorer.value) := v-1
            false
          case Some(next) =>
            val newValueForSuccValue =
              if(next.value < v) next.value - 1
              else next.value
            if(successorValues(explorer.value).newValue != newValueForSuccValue){
              successorValues(explorer.value) := newValueForSuccValue
              explorer = next
              true
            }else false
        }){}
    }
  }
}

*/