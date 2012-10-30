/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 26/10/12
 * Time: 13:50
 * To change this template use File | Settings | File Templates.
 */


package oscar.cbls.routing.neighborhood

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

import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.routing.{PositionInRouteAndRouteNr, ClosestNeighborPoints, VRP, ObjectiveFunction}


/**moves a point in a circuit to another place.
  * size if O(n²)
  */

object Swap extends SearchEngine{
  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,k:Int):Swap = findMove(false, vrp,k)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,k:Int , startFrom:Neighbor = null):Swap
  = findMove(true,vrp,k,startFrom)
  //def justMove(vrp:VRP with ObjectiveFunction, startFrom:Neighbor = null) {getFirstImprovingMove(vrp, startFrom).comit}


  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with ClosestNeighborPoints
                       with PositionInRouteAndRouteNr, k:Int,startFrom:Neighbor = null):Swap = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for (beforeFirstSwapPoint <- 0 until vrp.N startBy hotRestart if vrp.isRouted(beforeFirstSwapPoint)){
      val firstSwapPoint = vrp.Next(beforeFirstSwapPoint).value
      for(beforeSecondSwapPoint <- vrp.getKNearestNeighbors(k,firstSwapPoint) if vrp.isRouted(beforeSecondSwapPoint))
      {
        if (vrp.isASegment(beforeFirstSwapPoint,vrp.Next(firstSwapPoint).value) &&
          vrp.isASegment(beforeSecondSwapPoint,vrp.Next(beforeSecondSwapPoint).value) &&
          vrp.isASegment(vrp.Next(firstSwapPoint).value,beforeSecondSwapPoint))
        {
          val newObj = getObjAfterMove(beforeFirstSwapPoint,beforeSecondSwapPoint, vrp)
          if (newObj < BestObj){
            if (FirstImprove) return Swap(beforeFirstSwapPoint,beforeSecondSwapPoint, newObj, vrp)
            BestObj = newObj
            move = ((beforeFirstSwapPoint, beforeSecondSwapPoint))
          }
        }
      }
    }
    if (move == null) null
    else Swap(move._1,move._2, BestObj, vrp)
  }

  def doMove(predOfFirstSwapedPoint:Int, predOfSecondSwappedPoint:Int, vrp:VRP){
    val toUpdate =vrp.swap(predOfFirstSwapedPoint,vrp.Next(predOfFirstSwapedPoint).value,predOfSecondSwappedPoint,
      vrp.Next(predOfSecondSwappedPoint).value)
    toUpdate.foreach(t => t._1 := t._2)
  }

  /*
    Evaluate the objective after a temporary one-point-move action thanks to ObjectiveFunction's features.
   */
  def getObjAfterMove(predOfFirstSwapedPoint:Int, predOfSecondSwappedPoint:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate =vrp.swap(predOfFirstSwapedPoint,vrp.Next(predOfFirstSwapedPoint).value,predOfSecondSwappedPoint,
      vrp.Next(predOfSecondSwappedPoint).value)
    vrp.getAssignVal(toUpdate)
  }
}

case class Swap(val predOfMovedPoint:Int, val PutAfter:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {Swap.doMove(predOfMovedPoint, PutAfter, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(beforeFirstSwapped = " + predOfMovedPoint + ", beforeSecondSwapped = " + PutAfter+" )"

  def startNodeForNextExploration: Int = predOfMovedPoint
}

