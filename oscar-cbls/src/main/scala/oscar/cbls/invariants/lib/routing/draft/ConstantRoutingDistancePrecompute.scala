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

import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.routing.{ConstantRoutingDistance, RoutingConventionMethods}

object ConstantRoutingDistancePrecompute {

  def apply(routes : ChangingSeqValue,
            v : Int,
            perVehicle:Boolean,
            distanceMatrix : Array[Array[Int]],
            distanceIsSymmetric : Boolean):Array[CBLSIntVar] = {

    val distance:Array[CBLSIntVar] =
      if(perVehicle) Array.tabulate(v)(v => CBLSIntVar(routes.model,name="distanceOfVehicle" + v))
      else Array.fill(1)(CBLSIntVar(routes.model,name="overallDistance"))

    ConstantRoutingDistancePrecompute(routes,
      v,
      distanceMatrix,
      distance,
      distanceIsSymmetric)

    distance
  }

  def isDistanceSymmetric(distanceMatrix : Array[Array[Int]]):Boolean = {
    val n = distanceMatrix.length
    var i = 0
    while(i < n){
      var j = 0
      while(j <= i){
        if (distanceMatrix(i)(j) != distanceMatrix(j)(i))
          return false
        j += 1
      }
      i += 1
    }
    true
  }
}

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
case class ConstantRoutingDistancePrecompute(routes:ChangingSeqValue,
                                             v:Int,
                                             distanceMatrix:Array[Array[Int]],
                                             distance:Array[CBLSIntVar],
                                             distanceIsSymmetric:Boolean)
  extends ConstantRoutingDistance(routes:ChangingSeqValue,
    v:Int,
    distanceMatrix:Array[Array[Int]],
    distance:Array[CBLSIntVar],
    distanceIsSymmetric:Boolean) {

  val n = routes.maxValue + 1

  private val precomputedForwardCumulatedCostAtCheckpoint : Array[Int] = Array.fill(n)(0)
  private val precomputedBackwardCumulatedCostAtCheckpont : Array[Int] = Array.fill(n)(0)

  def computePrecomputedForwardCumulatedCostAtCheckpoint(fromValueExcluded : Int, toValueIncluded : Int, seq : IntSequence) {
    var explorerOPt = seq.explorerAtAnyOccurrence(fromValueExcluded).head.next
    var prevValue = fromValueExcluded
    while(explorerOPt match{
      case None => throw new Error("end of sequence reached before toValueIncluded?!")
      case Some(explorer) =>
        val value = explorer.value
        val position = explorer.position
        precomputedForwardCumulatedCostAtCheckpoint(value) =
          precomputedForwardCumulatedCostAtCheckpoint(prevValue) + distanceMatrix(prevValue)(value) + distanceMatrix(prevValue)(prevValue)
        explorerOPt = explorer.next
        prevValue = value
        value != toValueIncluded
    }){}
  }

  def computePrecomputedBackwardCumulatedCostAtCheckpoint(fromValueExcluded : Int, toValueIncluded : Int, seq : IntSequence) {
    var explorerOPt = seq.explorerAtAnyOccurrence(fromValueExcluded).head.next
    var prevValue = fromValueExcluded
    while(explorerOPt match{
      case None => throw new Error("end of sequence reached before toValueIncluded?!")
      case Some(explorer) =>
        val value = explorer.value
        val position = explorer.position
        precomputedBackwardCumulatedCostAtCheckpont(value) =
          precomputedBackwardCumulatedCostAtCheckpont(prevValue) + distanceMatrix(value)(prevValue) + distanceMatrix(value)(value)
        explorerOPt = explorer.next
        prevValue = value
        value != toValueIncluded
    }){}
  }


  override protected def computeValueBetween(s : IntSequence, fromPosIncluded : Int, toPosIncluded : Int) : Int = {
    if(s quickEquals checkpoint){
      if(fromPosIncluded < toPosIncluded){
        //Forward
        (precomputedForwardCumulatedCostAtCheckpoint(s.valueAtPosition(toPosIncluded).head)
          - precomputedForwardCumulatedCostAtCheckpoint(s.valueAtPosition(fromPosIncluded).head))
      }else {
        //backward
        (precomputedForwardCumulatedCostAtCheckpoint(s.valueAtPosition(fromPosIncluded).head)
          - precomputedForwardCumulatedCostAtCheckpoint(s.valueAtPosition(toPosIncluded).head))
      }
    }else {
      super.computeValueBetween(s, fromPosIncluded, toPosIncluded)
    }
  }

  override protected def saveCurrentCheckpoint(s : IntSequence) : Unit = {
    //records what has changed since last checkpoint, to update only these pre-computations
    super.saveCurrentCheckpoint(s)
    //update only the required pre-computations
  }
}
