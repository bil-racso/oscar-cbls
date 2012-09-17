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

object ThreeOptMove extends SearchEngine with BasicMoves{
  //def getBestMove(vrp:VRP with ObjectiveFunction):Neighbor = findMove(false, vrp)
  //def getFirstImprovingMove(vrp:VRP with ObjectiveFunction):Neighbor= findMove(true,vrp)

  /**search for the proper One point move
   *
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one
   * @param vrp the model of the problem
   */
  def findMove(FirstImprove:Boolean,
                       WithFlip:Boolean,
                       MaxSegmentSize:Int,
                       vrp:VRP with ObjectiveFunction):ThreeOptMove = {

    val range = vrp.V+1 until vrp.N+1
    var BestObj = Int.MaxValue
    var move:((Int, Int)) = null

    for (InsertionPoint <- 1 until vrp.N){

      for (BeforeSegmentStart <- 1 until vrp.N
           if vrp.Next(BeforeSegmentStart).value != 0 && vrp.Next(BeforeSegmentStart).value > vrp.V){

        var AfterLastSegmentPoint:Int = vrp.Next(BeforeSegmentStart).value
        var LastPositionOfInsertedSegment = InsertionPoint
        while(AfterLastSegmentPoint > vrp.V){
          //ajouter AfterLastSegmentPoint dans le segment déplacé
          //shifter AfterLastSegmentPoint

          LastPositionOfInsertedSegment = 0
        }
      }
    }

//      for (putAfter <- range if vrp.Next(BeforeMovedPoint).getValue(true) != putAfter && vrp.Next(putAfter).getValue(true) != BeforeMovedPoint){
//        val newObj = getObjAfterMove(BeforeMovedPoint,putAfter, vrp)
//        if (FirstImprove){
//          return OnePointMove(BeforeMovedPoint,putAfter, newObj, vrp)
//        }else if (newObj < BestObj){
//          BestObj = newObj
//          move = ((BeforeMovedPoint, putAfter))
//        }
//      }
    null
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
  //TODO: on risque de créer des tas de spurious cycles. ça va coûter cher vu que le topological sort sera non-incrémental du coup.
  def doMove(BeforeSegment: Int, EndOfSegment: Int, InsertionPoint: Int, vrp:VRP, FlipSegment:Boolean=false, withBackTrack:Boolean = false):Snapshot = {
    val oldstate = if(withBackTrack)
      vrp.m.saveValues(vrp.Next(BeforeSegment), vrp.Next(vrp.Next(BeforeSegment).value), vrp.Next(InsertionPoint))
    else null

    if (FlipSegment){
      flipSegment(BeforeSegment, EndOfSegment, vrp)
      moveSegment(BeforeSegment, EndOfSegment, InsertionPoint, vrp)
    }else{
      moveSegment(BeforeSegment, EndOfSegment, InsertionPoint, vrp)
    }

    oldstate
  }

  def getImprovement(beforeFrom:Int, to:Int, insertPoint:Int, vrp:VRP with ObjectiveFunction):Int = {

    val oldstate = vrp.m.saveValues(vrp.Next(beforeFrom), vrp.Next(to), vrp.Next(insertPoint))

    doMove(beforeFrom, to, insertPoint, vrp)

    val toReturn:Int = vrp.objective.value
    vrp.m.restoreSnapshot(oldstate)

    return toReturn
  }
}

case class ThreeOptMove(BeforeSegment:Int, EndOfSegment:Int, PutAfter:Int, FlipSegment: Boolean,
                        objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {ThreeOptMove.doMove(BeforeSegment, EndOfSegment, PutAfter, vrp, FlipSegment)}
  def getObjAfter = objAfter
}

