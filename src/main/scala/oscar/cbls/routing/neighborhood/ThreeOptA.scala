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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer and Florent Ghilain.
 ******************************************************************************/

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.modeling.Algebra._
import oscar.cbls.routing.model._

/**
 * Removes three edges of routes, and rebuilds routes from the segments. (without any reverse)
 *
 * Info : it also could be saw as the move of a route's segment to another place.
 * The search complexity is O(n³).
 */
object ThreeOptA extends SearchEngine{

  /**
   * Returns the best three-opt-move operator, i.e. which decreases the most the objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to a limited number of neighbors k of each points.
   *
   * Info: The search complexity is then O(nk²)
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node. Initially returns the full set of points of the VRP.
   * @return the best three-opt-move operator.
   */
  def getBestMove(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr,
    relevantNeighborhoodOfNode:Int=> Iterable[Int] = null):Neighbor =
      findMove(false, vrp, if(relevantNeighborhoodOfNode==null) _=>vrp.Nodes else relevantNeighborhoodOfNode,null,vrp.N)

  /**
   * Returns the first three-opt-move operator which decreases the actual objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to a limited number of neighbors k of each points.
   *
   * Info: The search complexity is then O(nk²)
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node. Initially returns the full set of points of the VRP.
   * @param startFrom specifies the starting point of the search procedure.
   * @return a three-opt-move operator improving objective.
   */
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr,
    relevantNeighborhoodOfNode:Int=> Iterable[Int] = null, startFrom:Neighbor = null):Neighbor =
      findMove(true,vrp, if(relevantNeighborhoodOfNode==null) _=>vrp.Nodes else relevantNeighborhoodOfNode, startFrom,vrp.N)

  /**
   * Returns the best three-opt-move operator with a restriction on the length of the moved segment,
   * i.e. which decreases the most the objective value of a given VRP problem.
   * The search 's complexity can be improve by restricting the search procedure
   * to a limited number of neighbors k of each points.
   *
   * Info: The search complexity is then O(nk²)
   * Put lengthRestricted to 3, or 4 to get a OR-opt move.
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node. Initially returns the full set of points of the VRP.
   * @param lengthRestricted the length restriction on the moved segment.
   * @return the best three-opt-move operator.
   */
  def getBestMoveRestricted(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr
    ,relevantNeighborhoodOfNode:Int=> Iterable[Int] = null,lengthRestricted:Int):Neighbor =
      findMove(false, vrp, if(relevantNeighborhoodOfNode==null) _=>vrp.Nodes else relevantNeighborhoodOfNode,null,lengthRestricted)


  /**
   * Returns the first three-opt-move operator with a restriction on the length of the moved segment,
   * i.e. which decreases the actual objective value of a given VRP problem.
   * The search 's complexity can be improve by restricting the search procedure
   * to a limited number of neighbors k of each points.
   *
   * Info: The search complexity is then O(nk²)
   * Put lengthRestricted to 3, or 4 to get a OR-opt move.
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node. Initially returns the full set of points of the VRP.
   * @param startFrom specifies the starting point of the search procedure.
   * @param lengthRestricted the length restriction on the moved segment.
   * @return a three-opt-move operator improving objective.
   */
  def getFirstImprovingMoveRestricted(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr,
    relevantNeighborhoodOfNode:Int=> Iterable[Int] = null, startFrom:Neighbor = null,lengthRestricted:Int):Neighbor =
      findMove(true,vrp, if(relevantNeighborhoodOfNode==null) _=>vrp.Nodes else relevantNeighborhoodOfNode, startFrom,lengthRestricted)

  /**
   * Search procedure of a proper three-opt-move operator in a given VRP problem.
   * Desired characteristics of the operator are given as parameter.
   * The search 's complexity can be improve by restricting the search procedure
   * to a limited number of neighbors k of each points.
   *
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one.
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node.
   * @param startFrom specifies the starting point of the search procedure.
   * @param limitLength the length restriction on the moved segment.
   * @return the proper three-opt-move operator specified by the parameters.
   */
  def findMove(FirstImprove:Boolean,
               vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr,
               relevantNeighborhoodOfNode:Int=> Iterable[Int], startFrom:Neighbor = null,limitLength:Int):ThreeOptA = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int, Int)) = null

    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration
    for (insertionPoint <- 0 until vrp.N startBy hotRestart if vrp.isRouted(insertionPoint)){
      //we search for a segment,
      // its start should be "close" to the insertion point
      //its end should be close to the next of the insertion point
      //begin and end should be on the same route and in this order
      for (beforeSegmentStart <- relevantNeighborhoodOfNode(insertionPoint)  if (insertionPoint != beforeSegmentStart)
        && vrp.isRouted(beforeSegmentStart))
      {
        for (segmentEnd <- relevantNeighborhoodOfNode(vrp.Next(insertionPoint).value)
             if(vrp.isRouted(segmentEnd) &&
               segmentEnd != insertionPoint &&
               vrp.isAtLeastAsFarAs(beforeSegmentStart, segmentEnd,2) &&
               !vrp.isBetween(insertionPoint, beforeSegmentStart, segmentEnd) &&
               vrp.isAtMostAsFarAs(beforeSegmentStart,segmentEnd,limitLength+1)))
        {
          val newObj = getObjAfterMove(beforeSegmentStart ,segmentEnd, insertionPoint, vrp)
          if (newObj < BestObj){
            if (FirstImprove)
              return ThreeOptA(beforeSegmentStart ,segmentEnd, insertionPoint, newObj, vrp)
            BestObj = newObj
            move = ((beforeSegmentStart ,segmentEnd, insertionPoint))
          }
        }
      }
    }
    if (move == null) null
    else ThreeOptA(move._1, move._2, move._3, BestObj, vrp)
  }

  /**
   * Performs a three-opt-move operator on a given VRP problem.
   * @param beforeFrom the predecessor of the moved segment.
   * @param to the end of the moved segment.
   * @param insertPoint the place where to insert the moved segment.
   * @param vrp the given VRP problem.
   */
  def doMove(beforeFrom: Int, to: Int, insertPoint: Int, vrp:VRP){
    val toUpdate = vrp.threeOptA(insertPoint,vrp.Next(insertPoint).value,beforeFrom,
      vrp.Next(beforeFrom).value,to,vrp.Next(to).value)
    toUpdate.foreach(t => t._1 := t._2)
  }

  /**
   * Evaluates and returns the objective after a temporary three-opt-move operator
   * thanks to ObjectiveFunction's features.
   * @param beforeFrom the predecessor of the moved segment.
   * @param to the end of the moved segment.
   * @param insertPoint the place where to insert the moved segment.
   * @param vrp the given VRP problem.
   * @return the objective value if we performed this three-opt-move operator.
   */
  def getObjAfterMove(beforeFrom:Int, to:Int, insertPoint:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.threeOptA(insertPoint,vrp.Next(insertPoint).value,beforeFrom,
      vrp.Next(beforeFrom).value,to,vrp.Next(to).value)
    vrp.assignVal(toUpdate)
  }
}

/**
 * Models a three-opt-move operator of a given VRP problem.
 * @param beforeSegmentStart the predecessor of the moved segment.
 * @param segmentEnd the end of the moved segment.
 * @param insertionPoint the place where to insert the moved segment.
 * @param objAfter the objective value if we performed this three-opt-move operator.
 * @param vrp the given VRP problem.
 */
case class ThreeOptA(beforeSegmentStart:Int, segmentEnd:Int, insertionPoint:Int,
                        objAfter:Int, vrp:VRP) extends Neighbor{
  // overriding methods
  def comit() {ThreeOptA.doMove(beforeSegmentStart, segmentEnd, insertionPoint, vrp)}
  def getObjAfter = objAfter
  def startNodeForNextExploration: Int = insertionPoint
  def getValuesToAssign = vrp.threeOptA(insertionPoint,vrp.Next(insertionPoint).value,beforeSegmentStart,
    vrp.Next(beforeSegmentStart).value,segmentEnd,vrp.Next(segmentEnd).value)

  override def toString:String = "TreeOptA(beforeStart = " + beforeSegmentStart + ", end = " + segmentEnd + ", insertion ="+ insertionPoint+" )"
}

