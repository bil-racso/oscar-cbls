package oscar.cbls.invariants.lib.routing.draft

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
import oscar.cbls.invariants.lib.routing.ConstantRoutingDistance

/**
 * @param routes the routes of all the vehicles
 * @param v the number of vehicles in the model
 * @param distanceMatrix the matrix of distance, which is expected to be symmetric
 * @param distance it is either an array of one value, in which case it is the total distance run by all vehicle
 *                 or it can also be an array of size v. in this case it is the distance run by each vehicle, respectively.
 *                 the second option is computationally more expensive
 * @param distanceIsSymmetric true if you swear that the distance matrix is symmetric, false and it will be considered as asymmetric (slower!)
 *
 * The distance computed by this invariant considers the values o the diagonal as part of the cost (node cost are added to the distance)
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class ConstantRoutingDistancePrecompute(routes:ChangingSeqValue,
                                        v:Int,
                                        distanceMatrix:Array[Array[Int]],
                                        distance:Array[CBLSIntVar],
                                        distanceIsSymmetric:Boolean,
                                        precomputeFW:Boolean,
                                        precomputeBW:Boolean)
  extends ConstantRoutingDistance(routes:ChangingSeqValue,
    v:Int,
    distanceMatrix:Array[Array[Int]],
    distance:Array[CBLSIntVar],
    distanceIsSymmetric:Boolean) {

  val n = routes.maxValue + 1

  val isFWPrecomputeUpToDate:Array[Boolean] = Array.fill(v)(false)
  val isBWPrecomputeUpToDate:Array[Boolean] = Array.fill(v)(false)
  private val precomputedForwardCumulatedCostAtCheckpoint : Array[Int] = if(precomputeFW) Array.fill(n)(0) else null
  private val precomputedBackwardCumulatedCostAtCheckpont : Array[Int] = if(precomputeBW) Array.fill(n)(0) else null

  def computePrecomputedForwardCumulatedCostAtCheckpoint(vehicle : Int, seq : IntSequence) {
    var explorerOPt = seq.explorerAtAnyOccurrence(vehicle).head.next
    var prevValue = vehicle
    while(explorerOPt match{
      case None => false //we finished the last vehicle
      case Some(explorer) =>
        val value = explorer.value
        if(value != vehicle+1){
          precomputedForwardCumulatedCostAtCheckpoint(value) =
            precomputedForwardCumulatedCostAtCheckpoint(prevValue) +
              distanceMatrix(prevValue)(value) +
              distanceMatrix(prevValue)(prevValue)
          explorerOPt = explorer.next
          prevValue = value
          true
        } else false
    }){}
  }

  def computePrecomputedBackwardCumulatedCostAtCheckpoint(vehicle : Int, seq : IntSequence) {
    var explorerOPt = seq.explorerAtAnyOccurrence(vehicle).head.next
    var prevValue = vehicle
    while(explorerOPt match{
      case None => throw new Error("end of sequence reached before toValueIncluded?!")
      case Some(explorer) =>
        val value = explorer.value
        if(value != vehicle+1) {
          precomputedBackwardCumulatedCostAtCheckpont(value) =
            precomputedBackwardCumulatedCostAtCheckpont(prevValue) +
              distanceMatrix(value)(prevValue) +
              distanceMatrix(value)(value)
          explorerOPt = explorer.next
          prevValue = value
          true
        } else false
    }){}
  }

  override protected def computeValueBetween(s : IntSequence, vehicle:Int,
                                             fromPosIncluded : Int, fromValueIncluded:Int,
                                             toPosIncluded : Int, toValueIncluded:Int) : Int = {
    val atCheckpoint = s quickEquals checkpoint
    val forwardRequired = fromPosIncluded < toPosIncluded

    if(fromPosIncluded == toPosIncluded) {
      distanceMatrix(fromValueIncluded)(fromValueIncluded)
    } else if(atCheckpoint && precomputeFW && forwardRequired){
      //Forward
      doFWPrecomputeForVehicle(vehicle)
      val toReturn = (precomputedForwardCumulatedCostAtCheckpoint(toValueIncluded)
        + distanceMatrix(fromValueIncluded)(fromValueIncluded)
        - precomputedForwardCumulatedCostAtCheckpoint(fromValueIncluded))
      assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded,toPosIncluded,toValueIncluded))
      toReturn
    }else if(atCheckpoint && precomputeBW && !forwardRequired) {
      //backward
      doBWPrecomputeForVehicle(vehicle)
      val toReturn = (precomputedBackwardCumulatedCostAtCheckpont(fromValueIncluded)
        + distanceMatrix(toValueIncluded)(toValueIncluded)
        - precomputedBackwardCumulatedCostAtCheckpont(toValueIncluded))
      assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded, toPosIncluded, toValueIncluded))
      toReturn
    }else if(atCheckpoint && (precomputeFW || precomputeBW) && distanceIsSymmetric){
      //gt the other distance from the available pre-compute
      if(precomputeFW){
        //getting a BW distance from a FW precompute
        doFWPrecomputeForVehicle(vehicle)
        val toReturn = (precomputedForwardCumulatedCostAtCheckpoint(fromValueIncluded)
          + distanceMatrix(toValueIncluded)(toValueIncluded)
          - precomputedForwardCumulatedCostAtCheckpoint(toValueIncluded))
        assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded,toPosIncluded,toValueIncluded))
        toReturn
      }else{
        //getting a FW distance from a BW precompute
        doBWPrecomputeForVehicle(vehicle)
        val toReturn = (precomputedBackwardCumulatedCostAtCheckpont(toValueIncluded)
          + distanceMatrix(fromValueIncluded)(fromValueIncluded)
          - precomputedBackwardCumulatedCostAtCheckpont(fromValueIncluded))
        assert(toReturn == super.computeValueBetween(s, vehicle, fromPosIncluded, fromValueIncluded, toPosIncluded, toValueIncluded))
        toReturn
      }
    }else{
      super.computeValueBetween(s, vehicle:Int, fromPosIncluded, fromValueIncluded,toPosIncluded,toValueIncluded)
    }
  }

  def doFWPrecomputeForVehicle(vehicle:Int){
    if(isFWPrecomputeUpToDate(vehicle)) return
    computePrecomputedForwardCumulatedCostAtCheckpoint(vehicle,checkpoint)
    isFWPrecomputeUpToDate(vehicle) = true
  }

  def doBWPrecomputeForVehicle(vehicle:Int){
    if(isBWPrecomputeUpToDate(vehicle)) return
    computePrecomputedBackwardCumulatedCostAtCheckpoint(vehicle,checkpoint)
    isBWPrecomputeUpToDate(vehicle) = true
  }

  override protected def saveCurrentCheckpoint(s : IntSequence) : Unit = {
    //records what has changed since last checkpoint, to update only these pre-computations, lazily

    var routesToPrecompute = touchedRoutesSinceCheckpointList
    while(routesToPrecompute != null){
      val currentRoute=routesToPrecompute.head
      routesToPrecompute = routesToPrecompute.tail
      isFWPrecomputeUpToDate(currentRoute) = false
      isBWPrecomputeUpToDate(currentRoute) = false
    }

    super.saveCurrentCheckpoint(s)
  }
}
