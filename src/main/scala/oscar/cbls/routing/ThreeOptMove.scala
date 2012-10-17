package oscar.cbls.routing

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.search.SearchEngine
import oscar.cbls.invariants.core.computation.Snapshot

/**moves a segment to another place, without flipping it.
 * size is O(n³)
 */
object ThreeOptMove extends SearchEngine{
  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr, k:Int):Neighbor = findMove(false, vrp, k)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr, k:Int, prevmove:Neighbor = null):Neighbor= findMove(true,vrp, k, prevmove)

  /**search for the proper One point move
   *
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one
   * @param vrp the model of the problem
   */
  def findMove(FirstImprove:Boolean,
               vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
               k:Int, prevmove:Neighbor = null):ThreeOptMove = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int, Int)) = null

    def nextModulo(n:Int):Int = {
      if (n+1 >= vrp.N) 0
      else n+1
    }

    //TODO: BUG here: it sometimes generates a cycle.
    // no more cycle

    val hotRestart = if (prevmove == null) 0 else prevmove.startNodeForNextExploration
    for (insertionPoint <- RangeHotRestart(hotRestart,hotRestart+vrp.N)){
      //we search for a segment,
      // its start should be "close" to the insertion point
      //its end should be close to the next of the insertion point
      //begin and end should be on the same route and in this order
      for (beforeSegmentStart <- vrp.getKNearestNeighbors(k,insertionPoint)
        if insertionPoint != beforeSegmentStart ){
        for (segmentEnd <- vrp.getKNearestNeighbors(k,vrp.Next(insertionPoint).value)
             if beforeSegmentStart != segmentEnd &&
               segmentEnd != insertionPoint &&
               vrp.isASegment(beforeSegmentStart, segmentEnd) &&
               !vrp.isBetween(insertionPoint, beforeSegmentStart, segmentEnd)){
          val newObj = getObjAfterMove(beforeSegmentStart ,segmentEnd, insertionPoint, vrp)
          if (newObj < BestObj){
            if (FirstImprove){
              return ThreeOptMove(beforeSegmentStart ,segmentEnd, insertionPoint, newObj, vrp)
            }
            BestObj = newObj
            move = ((beforeSegmentStart ,segmentEnd, insertionPoint))
          }
        }
      }
    }
    if (move == null) null
    else ThreeOptMove(move._1, move._2, move._3, BestObj, vrp)
  }

  /*Performs the three-opt move without flip
   *
   * insert the segment Next(beforeFrom)--to after the insert point.
   * Next(beforeFrom)--to must be a segment of the same vehicle and cannot involve the starting point of the vehicle
   * @param beforeFrom is the starting point ofo the moved segment
   * @param to is the end point of the moved segment
   * @param insertPoint is the point where after the segment is inserted
   * @param vrp
   */

  def doMove(BeforeSegment: Int, EndOfSegment: Int, InsertionPoint: Int, vrp:VRP){
      val toUpdate = vrp.moveSegmentListToUpdate(BeforeSegment,EndOfSegment,InsertionPoint)
      toUpdate.foreach(t => t._1 := t._2)
  }

  /*
    Evaluate the objective after a temporary one-point-move action thanks to ObjectiveFunction's features.
   */
  def getObjAfterMove(beforeFrom:Int, to:Int, insertPoint:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.moveSegmentListToUpdate(beforeFrom,to,insertPoint)
    vrp.getAssignVal(toUpdate)
  }
}

case class ThreeOptMove(beforeSegmentStart:Int, segmentEnd:Int, insertionPoint:Int,
                        objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {ThreeOptMove.doMove(beforeSegmentStart, segmentEnd, insertionPoint, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "moved " + vrp.Next(beforeSegmentStart).value + "-...->" + segmentEnd + " after "+ insertionPoint

  def startNodeForNextExploration: Int = insertionPoint
}

