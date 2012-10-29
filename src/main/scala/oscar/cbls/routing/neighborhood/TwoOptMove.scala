/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 25/10/12
 * Time: 14:44
 * To change this template use File | Settings | File Templates.
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.routing.{PositionInRouteAndRouteNr, ClosestNeighborPoints, VRP, ObjectiveFunction}
import oscar.cbls.invariants.core.computation.IntVar


/**moves a point in a circuit to another place.
  * size if O(n²)
  */

object TwoOptMove extends SearchEngine{
  var toUpdate:List[(IntVar,Int)] = List.empty //list of variables to update if we get an intersting move
  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
                  ,k:Int):TwoOptMove = findMove(false, vrp,k)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
    k:Int,startFrom:Neighbor = null):TwoOptMove = findMove(true,vrp,k,startFrom)


  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with ClosestNeighborPoints
    with PositionInRouteAndRouteNr, k:Int,startFrom:Neighbor = null):TwoOptMove = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for (firstEdge <- 0 until vrp.N startBy hotRestart){
      for(secondEdge <- vrp.getKNearestNeighbors(k,firstEdge)
        if (vrp.Next(secondEdge).value>=vrp.V && vrp.isASegment(vrp.Next(firstEdge).value,secondEdge))){
          val newObj = getObjAfterMove(firstEdge,secondEdge, vrp)
          if (newObj < BestObj){
            if (FirstImprove) return TwoOptMove(firstEdge,secondEdge, newObj, vrp)
            BestObj = newObj
            move = ((firstEdge, secondEdge))
          }
      }
    }
    if (move == null) null
    else TwoOptMove(move._1,move._2, BestObj, vrp)

  }

  def doMove(firstEdge:Int, secondEdge:Int, vrp:VRP){
    toUpdate =vrp.twoOpt(firstEdge,vrp.Next(firstEdge).value,secondEdge,vrp.Next(secondEdge).value)
    toUpdate.foreach(t => t._1 := t._2)
  }

  /*
    Evaluate the objective after a temporary one-point-move action thanks to ObjectiveFunction's features.
   */
  def getObjAfterMove(firstEdge:Int, secondEdge:Int, vrp:VRP with ObjectiveFunction):Int = {
    toUpdate = vrp.twoOpt(firstEdge,vrp.Next(firstEdge).value,secondEdge,vrp.Next(secondEdge).value)
    vrp.getAssignVal(toUpdate)
  }
}

case class TwoOptMove(val predOfMovedPoint:Int, val PutAfter:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {TwoOptMove.doMove(predOfMovedPoint, PutAfter, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(point = " + vrp.Next(predOfMovedPoint).value + ", insertion = " + PutAfter+" )"

  def startNodeForNextExploration: Int = predOfMovedPoint
}

