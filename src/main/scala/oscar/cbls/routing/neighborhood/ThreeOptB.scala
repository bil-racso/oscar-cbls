/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 20/10/12
 * Time: 16:36
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

/*

 */
object ThreeOptB extends SearchEngine{


  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
     k:Int):Neighbor = findMove(false, vrp, k,null,vrp.N)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
    k:Int, prevmove:Neighbor = null):Neighbor= findMove(true,vrp, k, prevmove,vrp.N)

  def getBestMoveRestricted(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
   , k:Int,lengthRestricted:Int):Neighbor = findMove(false, vrp, k,null,lengthRestricted)
  def getFirstImprovingMoveRestricted(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints
    with PositionInRouteAndRouteNr, k:Int, prevmove:Neighbor = null,lengthRestricted:Int):Neighbor =
    findMove(true,vrp, k, prevmove,lengthRestricted)

  def findMove(FirstImprove:Boolean,
               vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
               k:Int, previousMove:Neighbor = null,limitLength:Int):ThreeOptB = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int, Int)) = null

    val hotRestart = if (previousMove == null) 0 else previousMove.startNodeForNextExploration
    var endOfFirstEdge:Int = 0

    for (startOfFirstEdge <- 0 until vrp.N startBy hotRestart if vrp.isRouted(startOfFirstEdge)){
      endOfFirstEdge = vrp.Next(startOfFirstEdge).value

      for (startOfThirdEdge <- vrp.getKNearestNeighbors(k,startOfFirstEdge)
           if vrp.isRouted(startOfThirdEdge))// filter
      {
        for (startOfSecondEdge <- vrp.getKNearestNeighbors(k,vrp.Next(startOfThirdEdge).value)
             if vrp.isRouted(startOfSecondEdge))
        {
          if ((vrp.isASegment(startOfFirstEdge,vrp.Next(startOfThirdEdge).value) &&
            vrp.isBetween(vrp.Next(startOfSecondEdge).value,startOfFirstEdge,startOfThirdEdge) &&
            vrp.isASegment(endOfFirstEdge,vrp.Next(startOfSecondEdge).value) &&
            vrp.isAtLeastAsFarAs(endOfFirstEdge,startOfThirdEdge,3) &&
            vrp.isAtMostAsFarAs(endOfFirstEdge,startOfThirdEdge,limitLength+1)) ||
            (!vrp.onTheSameRoute(startOfFirstEdge,startOfSecondEdge) &&
              vrp.onTheSameRoute(startOfSecondEdge,startOfThirdEdge) &&
              vrp.isAtLeastAsFarAs(startOfSecondEdge, vrp.Next(startOfThirdEdge).value,3) &&
              vrp.isAtMostAsFarAs(endOfFirstEdge,startOfThirdEdge,limitLength+1)))
          {
            val newObj = getObjAfterMove(startOfFirstEdge,startOfSecondEdge ,startOfThirdEdge,vrp)
            if (newObj < BestObj){
              if(FirstImprove)
                return ThreeOptB(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,newObj, vrp)
              BestObj = newObj
              move = ((startOfFirstEdge, startOfSecondEdge ,startOfThirdEdge ))
            }
          }
        }
      }
    }
    if (move == null) null
    else ThreeOptB(move._1, move._2, move._3, BestObj, vrp)
  }

  /*Performs the three-opt move with one reverse segment. */

  def doMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,vrp:VRP)  {
    val toUpdate = vrp.threeOptB(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    toUpdate.foreach(t => t._1 := t._2)
  }


  def getObjAfterMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.threeOptB(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    vrp.getAssignVal(toUpdate)
  }
}

case class ThreeOptB(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,
                                  objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {ThreeOptB.doMove(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(firstEdge = " + startOfFirstEdge + ", secondEdge = " + startOfSecondEdge + ", " +
    "thirdEdge = "+ startOfThirdEdge+" )"
  def startNodeForNextExploration: Int = startOfFirstEdge
}


