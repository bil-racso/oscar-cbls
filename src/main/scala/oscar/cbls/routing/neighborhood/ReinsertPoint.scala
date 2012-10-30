/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 28/10/12
 * Time: 15:31
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


/**moves a point in a circuit to another place.
  * size if O(n²)
  */

object ReinsertPoint extends SearchEngine{
  def getBestMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted):ReinsertPoint = findMove(false, vrp)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted, startFrom:Neighbor = null):ReinsertPoint
  = findMove(true,vrp,startFrom)


  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with PenaltyForUnrouted,
                       startFrom:Neighbor = null):ReinsertPoint = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for(reinsertedPoint <- vrp.Unrouted.value){
      for (beforeReinsertedPoint <- 0 until vrp.N startBy hotRestart if vrp.Next(beforeReinsertedPoint).value != vrp.N ){
        val newObj = getObjAfterMove(beforeReinsertedPoint,reinsertedPoint, vrp)
        if (newObj < BestObj){
          if (FirstImprove) return ReinsertPoint(beforeReinsertedPoint,reinsertedPoint, newObj, vrp)
          BestObj = newObj
          move = ((beforeReinsertedPoint, reinsertedPoint))
        }
      }
    }
    if(move != null) ReinsertPoint(move._1,move._2,BestObj,vrp)
    // if nothing improve objective, find the smallest increase of objective.
    BestObj = Int.MaxValue
    for(reinsertedPoint <- vrp.Unrouted.value){
      for (beforeReinsertedPoint <- 0 until vrp.N startBy hotRestart if vrp.Next(beforeReinsertedPoint).value != vrp.N ){
        val newObj = getObjAfterMove(beforeReinsertedPoint,reinsertedPoint, vrp)
        if (newObj < BestObj){
          if (FirstImprove) return ReinsertPoint(beforeReinsertedPoint,reinsertedPoint, newObj, vrp)
          BestObj = newObj
          move = ((beforeReinsertedPoint, reinsertedPoint))
        }
      }
    }

    if (move == null) null
    else ReinsertPoint(move._1,move._2, BestObj, vrp)
  }

    def doMove(beforeReinsertedPoint:Int, reinsertedPoint:Int, vrp:VRP){
    val toUpdate = vrp.add(beforeReinsertedPoint,reinsertedPoint)
    toUpdate.foreach(t => t._1 := t._2)
  }

  /*
    Evaluate the objective after a temporary one-point-move action thanks to ObjectiveFunction's features.
   */
  def getObjAfterMove(beforeReinsertedPoint:Int, reinsertedPoint:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.add(beforeReinsertedPoint,reinsertedPoint)
    vrp.getAssignVal(toUpdate)
  }
}

case class ReinsertPoint(val beforeReinsertedPoint:Int, val reinsertedPoint:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {ReinsertPoint.doMove(beforeReinsertedPoint, reinsertedPoint, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(beforeReinsertedPoint = " + beforeReinsertedPoint + ", reinsertedPoint = " + reinsertedPoint+" )"

  def startNodeForNextExploration: Int = beforeReinsertedPoint
}

