/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 28/10/12
 * Time: 14:29
 * To change this template use File | Settings | File Templates.
 */

/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 26/10/12
 * Time: 13:50
 * To change this template use File | Settings | File Templates.
 */



package oscar.cbls.routing.neighborhood
import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.routing._


/**moves a point in a circuit to another place.
  * size if O(n²)
  */

object RemovePointMove extends SearchEngine{
  def getBestMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted ,k:Int):RemovePointMove = findMove(false, vrp)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted, startFrom:Neighbor = null):RemovePointMove
  = findMove(true,vrp,startFrom)
  //def justMove(vrp:VRP with ObjectiveFunction, startFrom:Neighbor = null) {getFirstImprovingMove(vrp, startFrom).comit}


  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with PenaltyForUnrouted,
                       startFrom:Neighbor = null):RemovePointMove = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for (beforeRemovedPoint <- 0 until vrp.N startBy hotRestart if beforeRemovedPoint >= vrp.V){
      val removedPoint = vrp.Next(beforeRemovedPoint).value
      val newObj = getObjAfterMove(beforeRemovedPoint,removedPoint, vrp)
          if (newObj < BestObj){
            if (FirstImprove) return RemovePointMove(beforeRemovedPoint,removedPoint, newObj, vrp)
            BestObj = newObj
            move = ((beforeRemovedPoint, removedPoint))
          }
      }
    if (move == null) null
    else RemovePointMove(move._1,move._2, BestObj, vrp)
  }

  def doMove(beforeRemovedPoint:Int, removedPoint:Int, vrp:VRP){
    val toUpdate =vrp.remove(List((beforeRemovedPoint,removedPoint)))
    toUpdate.foreach(t => t._1 := t._2)
  }

  /*
    Evaluate the objective after a temporary one-point-move action thanks to ObjectiveFunction's features.
   */
  def getObjAfterMove(beforeRemovedPoint:Int, removedPoint:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate =vrp.remove(List((beforeRemovedPoint,removedPoint)))
    vrp.getAssignVal(toUpdate)
  }
}

case class RemovePointMove(val beforeRemovedPoint:Int, val removedPoint:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {RemovePointMove.doMove(beforeRemovedPoint, removedPoint, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(beforeRemovedPoint = " + beforeRemovedPoint + ", removedPoint = " + removedPoint+" )"

  def startNodeForNextExploration: Int = beforeRemovedPoint
}

