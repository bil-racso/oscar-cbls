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
import scala.util.Random


/**moves a point in a circuit to another place.
  * size if O(n²)
  */

object ReinsertPoint extends SearchEngine{
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted, startFrom:Neighbor = null):ReinsertPoint
  = findMove(true,false,vrp,startFrom)
  def getRandomMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted):ReinsertPoint = findMove(false,true,vrp)
  def getRandomMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted,startFrom:Neighbor,vehicle:Int):ReinsertPoint
    = findMove(false,true,vrp,startFrom,vehicle,true)

  def getBestMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted):ReinsertPoint = findMove(false,false, vrp)
  def getBestMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted,startFrom:Neighbor,vehicle:Int):ReinsertPoint
    = findMove(false,false, vrp,startFrom,vehicle,true)


  private def findMove(FirstImprove:Boolean,random:Boolean,vrp:VRP with ObjectiveFunction with PenaltyForUnrouted,
                       startFrom:Neighbor = null, vehicle:Int =0, onlyFrom:Boolean=false):ReinsertPoint = {
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) vehicle else startFrom.startNodeForNextExploration
    if(random){
      val beforeReinsertedPoint = if (onlyFrom) Range(hotRestart,hotRestart+1) else Random.shuffle(Range(0,vrp.N))
      for(p <- beforeReinsertedPoint if vrp.isRouted(p)){
        val toRoute = Random.shuffle(vrp.Unrouted.value)
        toRoute.foreach(i =>
          {
            val obj = getObjAfterMove(p,i,vrp)
            if(obj != Int.MaxValue){
              println(obj)
              println(Int.MaxValue)
              move = (p,i)
              return ReinsertPoint(move._1,move._2,obj,vrp)
            }
          })
      }
      return null
    }
    else{
      var BestObj:Int = vrp.ObjectiveVar.value
      var LeastWorstObj:Int = Int.MaxValue

      for (beforeReinsertedPoint <- if (!onlyFrom) (0 until vrp.N startBy hotRestart) else Range(hotRestart,hotRestart+1)
        if vrp.isRouted(beforeReinsertedPoint)){
          for(reinsertedPoint <- vrp.Unrouted.value){
            val newObj = getObjAfterMove(beforeReinsertedPoint,reinsertedPoint, vrp)
            if (newObj < BestObj){
              if (FirstImprove) return ReinsertPoint(beforeReinsertedPoint,reinsertedPoint, newObj, vrp)
              BestObj = newObj
              move = (beforeReinsertedPoint, reinsertedPoint)
            }
            else if (!FirstImprove && newObj < LeastWorstObj && BestObj == vrp.ObjectiveVar.value){
              LeastWorstObj = newObj
              move = (beforeReinsertedPoint, reinsertedPoint)
             }
          }
        }
      if (move == null) null
      else ReinsertPoint(move._1,move._2, if(BestObj!= vrp.ObjectiveVar.value) BestObj else LeastWorstObj, vrp)
   }
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

  def startNodeForNextExploration: Int = reinsertedPoint


}

