/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 25/10/12
 * Time: 14:44
 * To change this template use File | Settings | File Templates.
 */
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
  */

object TwoOpt extends SearchEngine{
  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
                  ,k:Int):TwoOpt = findMove(false, vrp,k)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
    k:Int,startFrom:Neighbor = null):TwoOpt = findMove(true,vrp,k,startFrom)


  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with ClosestNeighborPoints
    with PositionInRouteAndRouteNr, k:Int,startFrom:Neighbor = null):TwoOpt = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for (firstEdge <- 0 until vrp.N startBy hotRestart if vrp.isRouted(firstEdge)){
      for(secondEdge <- vrp.getKNearestNeighbors(k,firstEdge) if ((secondEdge!= firstEdge)
          && firstEdge!=vrp.Next(secondEdge).value &&  secondEdge!=vrp.Next(firstEdge).value)
          && vrp.onTheSameRoute(firstEdge,secondEdge))
      {
        val newObj = getObjAfterMove(firstEdge,secondEdge, vrp)
        if (newObj < BestObj){
          if (FirstImprove) return TwoOpt(firstEdge,secondEdge, newObj, vrp)
          BestObj = newObj
          move = ((firstEdge, secondEdge))
        }
      }
    }
    if (move == null) null
    else TwoOpt(move._1,move._2, BestObj, vrp)
  }

  def doMove(firstEdge:Int, secondEdge:Int, vrp:VRP){
    val toUpdate =vrp.twoOpt(firstEdge,vrp.Next(firstEdge).value,secondEdge,vrp.Next(secondEdge).value)
    toUpdate.foreach(t => t._1 := t._2)
  }


  /*
    Evaluate the objective after a temporary one-point-move action thanks to ObjectiveFunction's features.
   */
  def getObjAfterMove(firstEdge:Int, secondEdge:Int, vrp:VRP with ObjectiveFunction ):Int = {
    val toUpdate = vrp.twoOpt(firstEdge,vrp.Next(firstEdge).value,secondEdge,vrp.Next(secondEdge).value)
    vrp.getAssignVal(toUpdate)
   }
}

case class TwoOpt(val predOfMovedPoint:Int, val PutAfter:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {TwoOpt.doMove(predOfMovedPoint, PutAfter, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(point = " + vrp.Next(predOfMovedPoint).value + ", insertion = " + PutAfter+" )"

  def startNodeForNextExploration: Int = predOfMovedPoint
}

