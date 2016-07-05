package oscar.cbls.routing.seq.neighborhood

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

import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.search.{HotRestart, Pairs}
import oscar.cbls.routing.seq.model.VRP
import oscar.cbls.search.core.EasyNeighborhood

import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * swaps segments of different vehicles
 * THIS IS EXPERIMENTAL
 */
//TODO: add the possibilioty to inbhibit exchange with seg flip.
case class SegmentExchange(val vrp: VRP,
                           relevantNeighbors:()=>Int=>Iterable[Int], //must be routed
                           vehicles:() => List[Int],
                           neighborhoodName:String = "SegmentExchange",
                           hotRestart:Boolean = true,
                           best:Boolean = false)
  extends EasyNeighborhood[ThreeOptMove](best,neighborhoodName) {

  var firstSegmentStartPosition:Int = -1
  var firstSegmentEndPosition:Int = -1
  var flipFirstSegment:Boolean = false
  var secondSegmentStartPosition: Int = -1
  var secondSegmentEndPosition: Int = -1
  var flipSecondSegment:Boolean = false
  var startVehicle = 0

  val v = vrp.v
  val seq = vrp.routes

  override def exploreNeighborhood() {

    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    var listOfVehiclesToIterateOn = (if (hotRestart && !best) HotRestart(vehicles(), startVehicle) else vehicles()).toList
    var allVehiclesToIterateOn = SortedSet.empty[Int] ++ listOfVehiclesToIterateOn

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToCurrentCheckpoint(seqValue)
      a
    }

    val relevantNeighborsNow = relevantNeighbors()

    val nodeToRoute:RedBlackTreeMap[Int] = null

    while(listOfVehiclesToIterateOn.nonEmpty){
      val firstVehicle = listOfVehiclesToIterateOn.head
      listOfVehiclesToIterateOn = listOfVehiclesToIterateOn.tail
      allVehiclesToIterateOn = allVehiclesToIterateOn - firstVehicle

      val routeOfVehicle1 = vrp.getRouteOfVehicle(firstVehicle)

      var routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles:List[(Int,Int,Map[Int,Iterable[(Int,Int,Int)]])] = routeOfVehicle1.map(node =>
        (node, seqValue.positionOfAnyOccurrence(node).head, relevantNeighborsNow(node)
          .map(node => (node,if(node >=v) nodeToRoute.getOrElse(node,-1) else -1))
          .filter({case (node,routeNr) => node >= v && allVehiclesToIterateOn.contains(routeNr)})
          .map(nodeAndRoute => (nodeAndRoute._1,nodeAndRoute._2,seqValue.positionOfAnyOccurrence(nodeAndRoute._1).head))
          .groupBy(nodeAndRoute => nodeAndRoute._2))
      )

      while(routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles.nonEmpty) {
        val (firstNode, positionOfFistNode, firstNodeVehicleToNodeRoutePosition) = routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles.head
        var candidateForAfterEndOfFirstSegment = routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles.tail
        routeWithRelevantNeighborsTheirVehicleAndPositionGroupedByVehicles = candidateForAfterEndOfFirstSegment


        while (candidateForAfterEndOfFirstSegment.nonEmpty) {
          val (secondNode, positionOfSecondNode, secondNodeVehicleToNodeRoutePosition) = candidateForAfterEndOfFirstSegment.head
          candidateForAfterEndOfFirstSegment = candidateForAfterEndOfFirstSegment.tail

          //we define the first segment

          val isReversedFromFirstSecondNodesFirstSegment =
            if (positionOfFistNode < positionOfSecondNode) {
              firstSegmentStartPosition = positionOfFistNode + 1
              firstSegmentEndPosition = positionOfSecondNode - 1
              false
            } else {
              firstSegmentStartPosition = positionOfSecondNode + 1
              firstSegmentEndPosition = positionOfFistNode - 1
              true
            }

          if (firstSegmentStartPosition <= firstSegmentEndPosition) {

            //now we search for nodes in other vehicles
            val otherVehicles : Iterable[Int] = firstNodeVehicleToNodeRoutePosition.keys.filter((v : Int) => secondNodeVehicleToNodeRoutePosition.isDefinedAt(v))

            for (otherVehicle <- otherVehicles) {
              val relevantNeighborsForFirstNodeNodeVPos : Iterable[(Int, Int, Int)] = firstNodeVehicleToNodeRoutePosition(otherVehicle)
              val relevantNeighborsForSecondNodeNodeVPos : Iterable[(Int, Int, Int)] = secondNodeVehicleToNodeRoutePosition(otherVehicle)

              for ((relevantFirstNode, relevantFirstPos, _) <- relevantNeighborsForFirstNodeNodeVPos) {
                for ((relevantSecondNode, relevantSecondPos, _) <- relevantNeighborsForSecondNodeNodeVPos) {

                  val isReversedFromFirstSecondNodesSecondSegment =
                    if (relevantFirstPos < relevantSecondPos) {
                      secondSegmentStartPosition = relevantFirstPos + 1
                      secondSegmentEndPosition = relevantSecondPos - 1
                      false
                    } else {
                      secondSegmentStartPosition = relevantSecondPos + 1
                      secondSegmentEndPosition = relevantFirstPos - 1
                      true
                    }

                  val shouldFlipSegments = isReversedFromFirstSecondNodesSecondSegment != isReversedFromFirstSecondNodesFirstSegment
                  flipFirstSegment = shouldFlipSegments
                  flipFirstSegment = shouldFlipSegments

                  doMove(firstSegmentStartPosition, firstSegmentEndPosition, flipFirstSegment,
                    secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment)

                  if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
                    seq.releaseCurrentCheckpointAtCheckpoint()
                    startVehicle = firstVehicle +1
                    return
                  }

                }
              }
            } //end for otherVehicle
          } //end if (firstSegmentFomPosIncluded <= firstSegmentToPosIncluded)
        }//end loop on second node first segment
      }//end loop on first node first segment

    }//end loop on vehicles
    seq.releaseCurrentCheckpointAtCheckpoint()
  } //end def

  override def instantiateCurrentMove(newObj: Int): SegmentExchangeMove = {
    SegmentExchangeMove(
      firstSegmentStartPosition, firstSegmentEndPosition,flipFirstSegment,
      secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment,
      newObj, this, neighborhoodName)
  }

  def doMove(firstSegmentStartPosition:Int, firstSegmentEndPosition:Int, flipFirstSegment:Boolean,
             secondSegmentStartPosition: Int, secondSegmentEndPosition: Int, flipSecondSegment:Boolean){
    seq.swapSegments(firstSegmentStartPosition,
      firstSegmentEndPosition,
      flipFirstSegment,
      secondSegmentStartPosition,
      secondSegmentEndPosition,
      flipSecondSegment)
  }
}

case class SegmentExchangeMove(firstSegmentStartPosition:Int,
                               firstSegmentEndPosition:Int,
                               flipFirstSegment:Boolean,
                               secondSegmentStartPosition: Int,
                               secondSegmentEndPosition: Int,
                               flipSecondSegment:Boolean,
                               override val objAfter: Int,override val neighborhood:SegmentExchange,
                               override val neighborhoodName:String = "SegmentExchangeMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName,neighborhood.vrp){

  override def impactedPoints: Iterable[Int] =
    neighborhood.vrp.routes.value.valuesBetweenPositions(firstSegmentStartPosition,firstSegmentEndPosition) ++
      neighborhood.vrp.routes.value.valuesBetweenPositions(secondSegmentStartPosition,secondSegmentEndPosition)

  override def commit() {
    neighborhood.doMove(
      firstSegmentStartPosition,firstSegmentEndPosition, flipFirstSegment,
      secondSegmentStartPosition, secondSegmentEndPosition, flipSecondSegment)
  }

  override def toString: String = {
    neighborhoodNameToString + "SegmentExchange(firstSegmentStartPosition:" + firstSegmentStartPosition + " firstSegmentEndPosition:" + firstSegmentEndPosition + " flipFirstSegment:" + flipFirstSegment +
      " secondSegmentStartPosition:" + secondSegmentStartPosition + " secondSegmentEndPosition:" + secondSegmentEndPosition + " flipSecondSegment:" + flipSecondSegment + objToString + ")"
  }
}
