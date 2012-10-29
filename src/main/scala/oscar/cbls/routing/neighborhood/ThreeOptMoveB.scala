/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 20/10/12
 * Time: 16:36
 * To change this template use File | Settings | File Templates.
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.routing.{PositionInRouteAndRouteNr, ClosestNeighborPoints, VRP, ObjectiveFunction}

/*

 */
object ThreeOptMoveB extends SearchEngine{


  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
     k:Int):Neighbor = findMove(false, vrp, k)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
    k:Int, prevmove:Neighbor = null):Neighbor= findMove(true,vrp, k, prevmove)


  def findMove(FirstImprove:Boolean,
               vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
               k:Int, previousMove:Neighbor = null):ThreeOptMoveB = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int, Int)) = null


    val hotRestart = if (previousMove == null) 0 else previousMove.startNodeForNextExploration
    var endOfFirstEdge:Int = 0

    for (startOfFirstEdge <- 0 until vrp.N startBy hotRestart){
      endOfFirstEdge = vrp.Next(startOfFirstEdge).value

      for (startOfThirdEdge <- vrp.getKNearestNeighbors(k,startOfFirstEdge)
           if (vrp.isASegment(startOfFirstEdge,vrp.Next(startOfThirdEdge).value) &&
             vrp.isAtLeastAsFarAs(endOfFirstEdge,startOfThirdEdge,3))){// filter

        for (startOfSecondEdge <- vrp.getKNearestNeighbors(k,vrp.Next(startOfThirdEdge).value)
             if((vrp.isASegment(endOfFirstEdge,vrp.Next(startOfSecondEdge).value))&&
               vrp.isBetween(vrp.Next(startOfSecondEdge).value,startOfFirstEdge,startOfThirdEdge))){

          val newObj = getObjAfterMove(startOfFirstEdge,startOfSecondEdge ,startOfThirdEdge,vrp)
          if (newObj < BestObj){
            if(FirstImprove)
              return ThreeOptMoveB(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,newObj, vrp)
            BestObj = newObj
            move = ((startOfFirstEdge, startOfSecondEdge ,startOfThirdEdge ))
          }
        }
      }
    }
    if (move == null) null
    else ThreeOptMoveB(move._1, move._2, move._3, BestObj, vrp)
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

case class ThreeOptMoveB(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,
                                  objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {ThreeOptMoveB.doMove(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,vrp)}
  def getObjAfter = objAfter
  override def toString():String = "(firstEdge = " + startOfFirstEdge + ", secondEdge = " + startOfSecondEdge + ", " +
    "thirdEdge = "+ startOfThirdEdge+" )"
  def startNodeForNextExploration: Int = startOfFirstEdge
}


