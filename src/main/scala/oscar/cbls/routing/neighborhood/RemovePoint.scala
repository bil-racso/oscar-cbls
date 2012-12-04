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
import scala.util.Random

/**
 * Removes a point of route.
 * The search complexity is O(n).
 */
object RemovePoint extends SearchEngine{
  /**
   * Returns the best remove-point operator, i.e. which decreases the most the objective value
   * of a given VRP problem.
   * @param vrp the given VRP problem.
   * @return the best remove-point operator.
   */
  def getBestMove(vrp:VRP with ObjectiveFunction with Unrouted):RemovePoint = findMove(false,false, vrp)

  /**
   * Returns the first remove-point operator which decreases the actual objective value
   * of a given VRP problem. It allows us to specify a start point for the search procedure.
   * @param vrp the given VRP problem.
   * @param startFrom specifies the starting point of the search procedure.
   * @return a remove-point operator improving objective
   */
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with Unrouted, startFrom:Neighbor = null):RemovePoint
    = findMove(true,false,vrp,startFrom)

  /**
   * Returns a random remove-point operator of a give VRP problem.
   *
   * Info : the move could increase or decrease the objective value as it's a random move.
   * The complexity of this move is 0(1)
   * @param vrp the given VRP.
   * @return a random remove-point move.
   */
  def getRandomMove(vrp:VRP with ObjectiveFunction with Unrouted):RemovePoint = findMove(false,true,vrp)

  /**
   * Search procedure of a proper remove-point operator in a given VRP problem.
   * Desired characteristics of the operator are given as parameter.
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one.
   * @param random if true, returns a random move.
   * @param vrp the given VRP problem.
   * @param startFrom specifies the starting point of the search procedure.
   * @return the proper remove-point specified by the parameters.
   */
  private def findMove(FirstImprove:Boolean,random:Boolean,vrp:VRP with ObjectiveFunction with Unrouted,
                       startFrom:Neighbor = null):RemovePoint = {
    var move:((Int, Int)) = null
    if(random){
      val toUnroute = Random.shuffle(Range(vrp.V,vrp.N))
      for (beforeRemovePoint <- toUnroute if(vrp.isRouted(beforeRemovePoint))){
        val obj = getObjAfterMove(beforeRemovePoint,vrp.Next(beforeRemovePoint).value,vrp)
        if(obj != Int.MaxValue){
          move = (beforeRemovePoint,vrp.Next(beforeRemovePoint).value)
          return RemovePoint(move._1,move._2,obj,vrp)
        }
      }
      return null
    }
    else{
      var BestObj:Int = vrp.ObjectiveVar.value
      val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration
      for (beforeRemovedPoint <- 0 until vrp.N startBy hotRestart if(vrp.isRouted(beforeRemovedPoint) &&
        !vrp.isADepot(vrp.Next(beforeRemovedPoint).value)))
      {
        val removedPoint = vrp.Next(beforeRemovedPoint).value
        val newObj = getObjAfterMove(beforeRemovedPoint,removedPoint, vrp)
        if (newObj < BestObj){
          if (FirstImprove) return RemovePoint(beforeRemovedPoint,removedPoint, newObj, vrp)
          BestObj = newObj
          move = ((beforeRemovedPoint, removedPoint))
        }
      }
      if (move == null) null
      else RemovePoint(move._1,move._2, BestObj, vrp)
    }
  }

  /**
   * Performs a reinsert-point operator on a given VRP problem.
   * @param beforeRemovedPoint the predecessor of the point that will be removed.
   * @param removedPoint the point that will be removed.
   * @param vrp the given VRP problem.
   */
  def doMove(beforeRemovedPoint:Int, removedPoint:Int, vrp:VRP){
    val toUpdate =vrp.remove(List((beforeRemovedPoint,removedPoint)))
    toUpdate.foreach(t => t._1 := t._2)
  }


  /**
   * Evaluates and returns the objective after a temporary remove-point operator
   * thanks to ObjectiveFunction's features.
   * @param beforeRemovedPoint the predecessor of the point that will be removed.
   * @param removedPoint the point that will be removed.
   * @param vrp the given VRP problem.
   * @return the objective value if we performed this remove-point operator.
   */
  def getObjAfterMove(beforeRemovedPoint:Int, removedPoint:Int, vrp:VRP with ObjectiveFunction with Unrouted):Int = {
    val toUpdate = vrp.remove(List((beforeRemovedPoint,removedPoint)))
    vrp.getAssignVal(toUpdate)
 }
}

/**
 * Models a remove-point operator of a given VRP problem.
 * @param beforeRemovedPoint the predecessor of the point that will be removed.
 * @param removedPoint the point that will be removed.
 * @param objAfter the objective value if we performed this remove-point operator.
 * @param vrp the given VRP problem.
 */
case class RemovePoint(val beforeRemovedPoint:Int, val removedPoint:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  // overriding methods
  def comit {RemovePoint.doMove(beforeRemovedPoint, removedPoint, vrp)}
  def getObjAfter = objAfter
  def startNodeForNextExploration: Int = beforeRemovedPoint
  def getValuesToAssign = vrp.remove(List((beforeRemovedPoint,removedPoint)))

  override def toString():String = "(beforeRemovedPoint = " + beforeRemovedPoint + ", removedPoint = " + removedPoint+" )"
}

