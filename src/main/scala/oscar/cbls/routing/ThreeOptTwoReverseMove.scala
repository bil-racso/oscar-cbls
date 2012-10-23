/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 23/10/12
 * Time: 16:17
 * To change this template use File | Settings | File Templates.
 */

package oscar.cbls.routing

import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.invariants.core.computation.{IntVar, Snapshot}

/*

 */
object ThreeOptTwoReverseMove extends SearchEngine{


  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
    with OptimizeThreeOptWithReverse, k:Int):Neighbor = findMove(false, vrp, k)
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
    with OptimizeThreeOptWithReverse, k:Int, prevmove:Neighbor = null):Neighbor= findMove(true,vrp, k, prevmove)

  def findMove(FirstImprove:Boolean,
               vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr with OptimizeThreeOptWithReverse,
               k:Int, previousMove:Neighbor = null):ThreeOptTwoReverseMove = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int, Int)) = null


    val hotRestart = if (previousMove == null) 0 else previousMove.startNodeForNextExploration
    var endOfFirstEdge:Int = 0

    for (startOfFirstEdge <- 0 until vrp.N startBy hotRestart){
      endOfFirstEdge = vrp.Next(startOfFirstEdge).value

      for (startOfThirdEdge <- vrp.getKNearestNeighbors(k,endOfFirstEdge)
           if (vrp.isASegment(startOfFirstEdge,vrp.Next(startOfThirdEdge).value) &&
             vrp.isAtLeastAsFarAs(endOfFirstEdge,startOfThirdEdge,3))){// filter

        for (startOfSecondEdge <- vrp.getKNearestNeighbors(k,vrp.Next(startOfFirstEdge).value)
             if((vrp.isASegment(endOfFirstEdge,vrp.Next(startOfSecondEdge).value))&&
               vrp.isBetween(vrp.Next(startOfSecondEdge).value,startOfFirstEdge,startOfThirdEdge))){

          val newObj = getObjAfterMove(startOfFirstEdge,startOfSecondEdge ,startOfThirdEdge,vrp)
          if (newObj < BestObj){
            if(FirstImprove)
              return ThreeOptTwoReverseMove(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,newObj, vrp)
            BestObj = newObj
            move = ((startOfFirstEdge, startOfSecondEdge ,startOfThirdEdge ))
          }
        }
      }
    }
    if (move == null) null
    else ThreeOptTwoReverseMove(move._1, move._2, move._3, BestObj, vrp)
  }

  /*Performs the three-opt move with two reverse segment. */

  def doMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,vrp:VRP)  {
    val toUpdate = vrp.flipWith2ReverseListToUpdate(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    toUpdate.foreach(t => t._1 := t._2)
  }

  def getObjAfterMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int, vrp:VRP with ObjectiveFunction
    with OptimizeThreeOptWithReverse):Int = {
    val delta = vrp.getEffectivenessFlipWith2ReverseListToUpdate(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    vrp.ObjectiveVar.value + delta
  }
}

case class ThreeOptTwoReverseMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,
                                  objAfter:Int, vrp:VRP) extends Neighbor{
  def comit {ThreeOptTwoReverseMove.doMove(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,vrp)}
  def getObjAfter = objAfter
  override def toString():String = "moved " + startOfFirstEdge + "..." + startOfSecondEdge + "..."+ startOfThirdEdge
  def startNodeForNextExploration: Int = startOfFirstEdge
}


