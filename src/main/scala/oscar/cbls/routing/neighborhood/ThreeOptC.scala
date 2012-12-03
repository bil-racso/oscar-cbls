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
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.routing.model._

/**
 * Removes three edges of routes, and rebuilds routes from the segments. (with two reverses allowed)
 *
 * The search complexity is O(n³).
 */
object ThreeOptC extends SearchEngine{

  /**
   * Returns the best three-opt-move operator, i.e. which decreases the most the objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * Info: The search complexity is then O(nk²)
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @return the best three-opt-move operator.
   */
  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
    , k:Int):Neighbor = findMove(false, vrp, k)

  /**
   * Returns the first three-opt-move operator which decreases the actual objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * Info: The search complexity is then O(nk²)
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @param startFrom specifies the starting point of the search procedure.
   * @return a three-opt-move operator improving objective.
   */
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
    , k:Int, startFrom:Neighbor = null):Neighbor= findMove(true,vrp, k, startFrom)

  /**
   * Search procedure of a proper three-opt-move operator in a given VRP problem.
   * Desired characteristics of the operator are given as parameter.
   * The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one.
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @param startFrom specifies the starting point of the search procedure.
   * @return the proper three-opt-move operator specified by the parameters.
   */
  def findMove(FirstImprove:Boolean,
               vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
               k:Int, startFrom:Neighbor = null):ThreeOptC = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int, Int)) = null


    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration
    var endOfFirstEdge:Int = 0

    for (startOfFirstEdge <- 0 until vrp.N startBy hotRestart if (vrp.isRouted(startOfFirstEdge))){
      endOfFirstEdge = vrp.Next(startOfFirstEdge).value

      for (startOfThirdEdge <- vrp.getKNearestNeighbors(k,endOfFirstEdge)
           if ( vrp.isRouted(startOfThirdEdge)&&
             vrp.isASegment(startOfFirstEdge,vrp.Next(startOfThirdEdge).value) &&
             vrp.isAtLeastAsFarAs(endOfFirstEdge,startOfThirdEdge,3))){// filter

        for (startOfSecondEdge <- vrp.getKNearestNeighbors(k,vrp.Next(startOfFirstEdge).value)
             if ((startOfSecondEdge != endOfFirstEdge && vrp.Next(startOfSecondEdge).value != startOfThirdEdge) &&
               vrp.isRouted(startOfSecondEdge) &&
               (vrp.isASegment(endOfFirstEdge,vrp.Next(startOfSecondEdge).value))&&
               vrp.isBetween(vrp.Next(startOfSecondEdge).value,startOfFirstEdge,startOfThirdEdge)))
        {
          val newObj = getObjAfterMove(startOfFirstEdge,startOfSecondEdge ,startOfThirdEdge,vrp)
          if (newObj < BestObj){
            if(FirstImprove)
              return ThreeOptC(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,newObj, vrp)
            BestObj = newObj
            move = ((startOfFirstEdge, startOfSecondEdge ,startOfThirdEdge ))
          }
        }
      }
    }
    if (move == null) null
    else ThreeOptC(move._1, move._2, move._3, BestObj, vrp)
  }

  /**
   * Performs a three-opt-move operator on a given VRP problem.
   * @param startOfFirstEdge the start of first edge that we remove.
   * @param startOfSecondEdge the start of second edge that we remove.
   * @param startOfThirdEdge the start of third edge that we remove.
   * @param vrp the given VRP problem.
   */
  def doMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,vrp:VRP)  {
    val toUpdate = vrp.threeOptC(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    toUpdate.foreach(t => t._1 := t._2)
  }

  /**
   * Evaluates and returns the objective after a temporary three-opt-move operator
   * thanks to ObjectiveFunction's features.
   * @param startOfFirstEdge the start of first edge that we remove.
   * @param startOfSecondEdge the start of second edge that we remove.
   * @param startOfThirdEdge the start of third edge that we remove.
   * @param vrp the given VRP problem.
   * @return the objective value if we performed this three-opt-move operator.
   */
  def getObjAfterMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.threeOptC(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    vrp.getAssignVal(toUpdate)
  }
}

/**
 * Models a three-opt-move operator of a given VRP problem. *
 * @param startOfFirstEdge the start of first edge that we remove.
 * @param startOfSecondEdge the start of second edge that we remove.
 * @param startOfThirdEdge the start of third edge that we remove.
 * @param objAfter the objective value if we performed this three-opt-move operator.
 * @param vrp the given VRP problem.
 */
case class ThreeOptC(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,
                                  objAfter:Int, vrp:VRP) extends Neighbor{
  // overriding methods
  def comit {ThreeOptC.doMove(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,vrp)}
  def getObjAfter = objAfter
  def startNodeForNextExploration: Int = startOfFirstEdge

  override def toString():String =  "(firstEdge = " + startOfFirstEdge + ", secondEdge = " + startOfSecondEdge + ", " +
    "thirdEdge = "+ startOfThirdEdge+" )"
}


