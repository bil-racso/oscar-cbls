/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 28/10/12
 * Time: 15:31
 * To change this template use File | Settings | File Templates.
 */


package oscar.cbls.routing.neighborhood
import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.routing._


/**moves a point in a circuit to another place.
  * size if O(n²)
  */

object ReinsertPointMove extends SearchEngine{
  def getBestMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted ,k:Int):ReinsertPointMove = findMove(false, vrp)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted, startFrom:Neighbor = null):ReinsertPointMove
  = findMove(true,vrp,startFrom)
  //def justMove(vrp:VRP with ObjectiveFunction, startFrom:Neighbor = null) {getFirstImprovingMove(vrp, startFrom).comit}


  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with PenaltyForUnrouted,
                       startFrom:Neighbor = null):ReinsertPointMove = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for(reinsertedPoint <- vrp.Unrouted.value){
      for (beforeReinsertedPoint <- 0 until vrp.N startBy hotRestart){
        val newObj = getObjAfterMove(beforeReinsertedPoint,reinsertedPoint, vrp)
        if (newObj < BestObj){
          if (FirstImprove) return ReinsertPointMove(beforeReinsertedPoint,reinsertedPoint, newObj, vrp)
          BestObj = newObj
          move = ((beforeReinsertedPoint, reinsertedPoint))
        }
      }
    }
    if (move == null) null
    else ReinsertPointMove(move._1,move._2, BestObj, vrp)
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

case class ReinsertPointMove(val beforeReinsertedPoint:Int, val reinsertedPoint:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {ReinsertPointMove.doMove(beforeReinsertedPoint, reinsertedPoint, vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(beforeReinsertedPoint = " + beforeReinsertedPoint + ", reinsertedPoint = " + reinsertedPoint+" )"

  def startNodeForNextExploration: Int = beforeReinsertedPoint
}

