/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 26/10/12
 * Time: 13:50
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
import oscar.cbls.routing._
import scala.util.Random


/**moves a point in a circuit to another place.
  * size if O(n²)
  */

object RemovePoint extends SearchEngine{
  def getBestMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted):RemovePoint = findMove(false,false, vrp)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted, startFrom:Neighbor = null):RemovePoint
  = findMove(true,false,vrp,startFrom)
  def getRandomMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted):RemovePoint = findMove(false,true,vrp)


  private def findMove(FirstImprove:Boolean,random:Boolean,vrp:VRP with ObjectiveFunction with PenaltyForUnrouted,
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

  def doMove(beforeRemovedPoint:Int, removedPoint:Int, vrp:VRP){
    val toUpdate =vrp.remove(List((beforeRemovedPoint,removedPoint)))
    toUpdate.foreach(t => t._1 := t._2)
  }

   /*
    Evaluate the objective after a temporary one-point-move action thanks to ObjectiveFunction's features.
   */
  def getObjAfterMove(beforeRemovedPoint:Int, removedPoint:Int, vrp:VRP with ObjectiveFunction with PenaltyForUnrouted):Int = {
    val toUpdate = vrp.remove(List((beforeRemovedPoint,removedPoint)))
    vrp.getAssignVal(toUpdate)
 }
}

case class RemovePoint(val beforeRemovedPoint:Int, val removedPoint:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {RemovePoint.doMove(beforeRemovedPoint, removedPoint, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(beforeRemovedPoint = " + beforeRemovedPoint + ", removedPoint = " + removedPoint+" )"

  def startNodeForNextExploration: Int = beforeRemovedPoint
}

