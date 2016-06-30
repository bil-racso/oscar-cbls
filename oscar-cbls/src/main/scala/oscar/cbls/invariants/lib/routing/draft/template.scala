package oscar.cbls.invariants.lib.routing

/*
import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.invariants.core.computation.ChangingSeqValue
import oscar.cbls.invariants.core.computation.Invariant
import oscar.cbls.invariants.core.computation.SeqNotificationTarget
import oscar.cbls.invariants.core.computation.SeqUpdate
import oscar.cbls.invariants.core.computation.SeqUpdateDefineCheckpoint
import oscar.cbls.invariants.core.computation.SeqUpdateInsert
import oscar.cbls.invariants.core.computation.SeqUpdateLastNotified
import oscar.cbls.invariants.core.computation.SeqUpdateMove
import oscar.cbls.invariants.core.computation.SeqUpdateRemove
import oscar.cbls.invariants.core.computation.SeqUpdateRollBackToCheckpoint
import oscar.cbls.invariants.core.computation.SeqUpdateSet
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.core.propagation.{ErrorChecker, Checker}



/**
 * @param routes the routes of all the vehicles
 * @param v the number of vehicles in the model

 *
 * The distance computed by this invariant considers the values o the diagonal as part of the cost (node cost are added to the distance)
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class Temlplate(routes:ChangingSeqValue,
                                   v:Int)
  extends Invariant() with SeqNotificationTarget{


  registerStaticAndDynamicDependency(routes)
  finishInitialization()




  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    if(!digestUpdates(changes,false)) {


    }
  }

  private def digestUpdates(changes:SeqUpdate,skipNewCheckpoints:Boolean):Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive:Boolean) =>
        if(!digestUpdates(prev,true)){


        }

        true
      case SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) =>

        true
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev,skipNewCheckpoints)) return false

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev,skipNewCheckpoints)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          //this is a simple flip

        }else {
            //per vehicle, there might be some node cost to consider
            val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded, prev.newValue, v)
            val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after, prev.newValue, v)
            assert(vehicleOfMovedSegment == RoutingConventionMethods.searchVehicleReachingPosition(toIncluded, prev.newValue,v))

            if (vehicleOfMovedSegment == targetVehicleOfMove) {
              //the segment is moved to the same vehicle, so we do not consider node cost here



            } else {
              //moving a segment to another vehicle, and per vehicle required.

              //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)

            }
          }
          true
        }

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        val removedValue = x.removedValue
        //node cost to be considered
        if(!digestUpdates(prev,skipNewCheckpoints)) return false


        true

      case SeqUpdateLastNotified(value:IntSequence) =>


        true //we are starting from the previous value
      case SeqUpdateSet(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

}
*/