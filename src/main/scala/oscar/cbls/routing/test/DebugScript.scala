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
/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 3/10/12
 * Time: 13:13
 * To change this template use File | Settings | File Templates.
 */

package oscar.cbls.routing.test

import oscar.cbls.search.StopWatch
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.search.SearchEngine
import oscar.cbls.invariants.core.computation.IntVar._
import oscar.cbls.invariants.core.computation.{IntSetVar, IntVar, Model}
import oscar.cbls.invariants.lib.logic.{Cluster, IntVar2IntVarFun, Routes}
import oscar.cbls.invariants.lib.set.TakeAny._
import oscar.cbls.invariants.lib.set.TakeAny
import util.Random
import scala.math._
import oscar.cbls.routing._
import heuristic.NearestNeighbor
import neighborhood.{Neighbor, RemovePoint}

/**supports only a single vehicle*/
object DebugScript extends SearchEngine with App{


  val V:Int = 1
  val random = new Random(0)

  def getRandomDistanceMatrix(N:Int):Array[Array[Int]]= Array.tabulate(N,N)((i,j) => if (i==0 ||j == 0) 0 else random.nextInt(1000)+1)

  def getPlanarDistanceMatrix(coordX:Array[Int],coordY:Array[Int]):Array[Array[Int]] = {
    val N = coordX.length
    Array.tabulate(N,N)((i,j) => round(sqrt((   pow(coordX(i) - coordX(j), 2)
        + pow(coordY(i) - coordY(j), 2) ).toFloat)).toInt)
  }



  //val line = getPlanarDistanceMatrix(Array(1,2,3,4,5,6),Array(0,0,0,0,0,0))
  //val N:Int = 6

  //val line2 =  getPlanarDistanceMatrix(Array(1,2,3,4,5,6,-1),Array(0,0,0,0,0,0,0))
  val matrix = getPlanarDistanceMatrix(Array(0,1,2,3,4,5,6,7,8),Array(0,0,0,0,0,0,0,0,0))
  val N2:Int = 9
  var m: Model = new Model(false,false,false,false)
  var vrp = new VRP(N2, 1, m) with HopDistance with PositionInRouteAndRouteNr with ClosestNeighborPoints with PenaltyForUnrouted
    with OtherFunctionToObjective with Constraints

  vrp.installCostMatrix(matrix)
  vrp.fixUnroutedPenaltyWeight(8,-100)
  vrp.recordAddedFunctions(Array(vrp.UnroutedPenalty))
  println("matrix done")
  m.close()
  println("model close")
  //RandomNeighbor(vrp)
  NearestNeighbor(vrp)
  println(vrp.routes)
  println(vrp)
  m.propagate()
  println(vrp.routes)
  println(vrp)

  println("Objective = "+ vrp.ObjectiveVar.value)

  var move:Neighbor = null
  move = RemovePoint.getFirstImprovingMove(vrp, move)
  if(move.getObjAfter < vrp.ObjectiveVar.value)
    move.comit

  println(vrp.routes)
  println(vrp)

  println("Objective = "+ vrp.ObjectiveVar.value)




  //println("Debug route(" + N2 + "points,"+V+"cars)"+"\n Pred:")
 // println(vrp.preds)
 // println(vrp.Unrouted)
  // changement 1 vers 7
  // recherche

  // unroute the point 2
  //vrp.remove(List((vrp.preds(2).value,2))).foreach({t => t._1 := t._2})

  //vrp.remove(List( (vrp.preds(2).value,3) , (  vrp.preds(5).value ,5) )).foreach({t => t._1 := t._2})
  //vrp.threeOptB(0,1,2,3,5,6).foreach(t => t._1 := t._2)
  //m.propagate()

  //println(vrp.routes)
  //println(vrp.preds)
  //println(vrp.Unrouted)

  // unroute the point 3 and 4
 /* vrp.remove(List(1,3)).foreach({t => t._1 := t._2})
  m.propagate()

  println(vrp.routes)
  println(vrp.preds)
  println(vrp.Unrouted)

*/

  /*
  var z = 0
  do{
    m = new Model(false,true,false,false)
    vrp = new VRP(N2, 1, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPoints with PredAndUnrouted
    vrp.installCostMatrix(line2)
    m.close()
    //println("Debug route(" + N2 + "points,"+V+"cars)")
    //NearestNeighbor(vrp)
    RandomNeighbor(vrp)
    m.propagate()
    //println(vrp)
    //println(vrp.routes)
    println("start val: " + vrp.ObjectiveVar)

    var nsize = 20
    var saturated = false
    var move:Neighbor = null
    var it = 0
    while(!saturated){
      val oldobj:Int = vrp.ObjectiveVar.value
      move = ThreeOptA.getFirstImprovingMove(vrp,nsize, move)
      if (move != null && move.getObjAfter < oldobj){
        it +=1
        println("it: " + it + " " + move + " " + vrp.ObjectiveVar)
        move.comit
        vrp.ObjectiveVar.value
        println(vrp)
      }else{
        //      if (nsize == 40){
        saturated = true
        //      }else{
        //        nsize = 40
        //        println("GOING TO k=40")
        //      }
      }
    }

    println("done "+ z)


    z+=1
  }
  while(z<100 && vrp.ObjectiveVar.value == 14)

  */

}