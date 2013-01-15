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
import oscar.cbls.modeling.Algebra._
import oscar.cbls.routing.model._
import scala.util.Random
import java.util


/**
 * Inserts an unrouted point in a route.
 * The search complexity is O(n²).
 */
object ReinsertPoint extends SearchEngine{

  /**
   * Returns the first reinsert-point operator which decreases the actual objective value
   * of a given VRP problem.
   * @param vrp the given VRP problem.
   * @param startFrom specifies the starting point of the search procedure.
   * @return a reinsert-point operator improving objective
   */
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with Unrouted, startFrom:Neighbor = null):ReinsertPoint
  = findMove(true,false,vrp,startFrom)

  /**
   * Returns a random reinsert-point operator of a give VRP problem.
   *
   * Info : the move could increase or decrease the objective value as it's a random move.
   * The complexity of this move is 0(1).
   * @param vrp the given VRP problem.
   * @return a random reinsert-point move.
   */
  def getRandomMove(vrp:VRP with ObjectiveFunction with Unrouted):ReinsertPoint = findMove(false,true,vrp)

  /**
   * Returns a random reinsert-point operator with a specified place where reinsert in a given VRP problem.
   *
   * @param vrp the given VRP problem.
   * @param startFrom specifies the starting point of the search procedure.
   * @param vehicle allows us to specify on which route we want to add a point (if startFrom is null).
   *
   * Info : the move could increase or decrease the objective value as it's a random move.
   * The complexity of this move is 0(1).
   * @return a random reinsert-point operator of a given VRP problem.
   */
  def getRandomMove(vrp:VRP with ObjectiveFunction with Unrouted,startFrom:Neighbor,vehicle:Int):ReinsertPoint
    = findMove(false,true,vrp,startFrom,vehicle,true)

  /**
   * Returns the best reinsert-point operator, i.e. which decreases the most the objective value
   * of a given VRP problem.
   * @param vrp the given VRP problem.
   * @return the best reinsert-point operator.
   */
  def getBestMove(vrp:VRP with ObjectiveFunction with Unrouted):ReinsertPoint = findMove(false,false, vrp)

  /**
   * Returns the best reinsert-point operator with a specified place where reinsert, i.e. which decreases the most
   * the objective value of a given VRP problem.
   * @param vrp the given VRP problem.
   * @param startFrom specifies the starting point of the search procedure.
   * @param vehicle allows us to specify on which route we want to add a point (if startFrom is null).
   * @return the best reinsert-point operator.
   */
   def getBestMove(vrp:VRP with ObjectiveFunction with Unrouted,startFrom:Neighbor,vehicle:Int):ReinsertPoint
    = findMove(false,false, vrp,startFrom,vehicle,true)

  /**
   * Search procedure of a proper reinsert-point operator in a given VRP problem.
   * Desired characteristics of the operator are given as parameter.
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one.
   * @param random if true, returns a random move.
   * @param vrp the given VRP problem.
   * @param startFrom specifies the starting point of the search procedure.
   * @param vehicle allows us to specify on which route we want to add a point (if startFrom is null).
   * @param onlyFrom if true, reduces the insertion place to a single element.
   * @return the proper reinsert-point specified by the parameters.
   */
  private def findMove(FirstImprove:Boolean,random:Boolean,vrp:VRP with ObjectiveFunction with Unrouted,
                       startFrom:Neighbor = null, vehicle:Int =0, onlyFrom:Boolean=false):ReinsertPoint = {
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) vehicle else startFrom.startNodeForNextExploration
    val rand = new Random()
    // if we want a random reinsert-move.
    if(random){
      val beforeReinsertedPoint = if (onlyFrom) List(hotRestart) else rand.shuffle((0 to vrp.N-1).toList)
      for(p <- beforeReinsertedPoint if vrp.isRouted(p)){
        val toRoute = Random.shuffle(vrp.Unrouted.value.toList)
        toRoute.foreach(i =>
          {
            val obj = getObjAfterMove(p,i,vrp)
            if(obj != Int.MaxValue){
              move = (p,i)
              return ReinsertPoint(move._1,move._2,obj,vrp)
            }
          })
      }
      null
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

  /**
   * Performs a reinsert-point operator on a given VRP problem.
   * @param beforeReinsertedPoint the place where insert an unrouted point.
   * @param reinsertedPoint an unrouted point.
   * @param vrp the given VRP problem.
   */
  def doMove(beforeReinsertedPoint:Int, reinsertedPoint:Int, vrp:VRP){
    val toUpdate = vrp.add(beforeReinsertedPoint,reinsertedPoint)
    toUpdate.foreach(t => t._1 := t._2)
  }

  /**
   *  Evaluates and returns the objective after a temporary reinsert-point operator
   * thanks to ObjectiveFunction's features.
   * @param beforeReinsertedPoint the place where insert an unrouted point.
   * @param reinsertedPoint an unrouted point.
   * @param vrp the given VRP problem.
   * @return the objective value if we performed this reinsert-point operator.
   */
  def getObjAfterMove(beforeReinsertedPoint:Int, reinsertedPoint:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.add(beforeReinsertedPoint,reinsertedPoint)
    vrp.assignVal(toUpdate)
  }
}

/**
 * Models a reinsert-point operator of a given VRP problem.
 * @param beforeReinsertedPoint the place where insert an unrouted point.
 * @param reinsertedPoint an unrouted point.
 * @param objAfter the objective value if we performed this reinsert-point operator.
 * @param vrp the given VRP problem.
 */
case class ReinsertPoint(beforeReinsertedPoint:Int, reinsertedPoint:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  // overriding methods
  def comit {ReinsertPoint.doMove(beforeReinsertedPoint, reinsertedPoint, vrp)}
  def getObjAfter = objAfter
  def startNodeForNextExploration: Int = reinsertedPoint
  def getValuesToAssign = vrp.add(beforeReinsertedPoint,reinsertedPoint)

  override def toString():String = "(beforeReinsertedPoint = " + beforeReinsertedPoint + ", reinsertedPoint = " + reinsertedPoint+" )"
}

