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
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.routing

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
import oscar.cbls.algebra.Algebra._

/**supports only a single vehicle*/
object tspsolver extends SearchEngine with StopWatch with App{

  // tester si l'objectif est bien la distance réel via la matrice de base des distances.
  def realSum(vrp:VRP,matrice:Array[Array[Int]])={
    var sum:Int = 0
    for(i<- 0 until vrp.Next.length){
      val j = vrp.Next(i).value

      sum += matrice(i)(j)
    }
    sum
  }
  val N:Int = 1000

  this.startWatch()
  
  println("TSP(" + N + ")")
  val random = new Random(0)
  
  def getRandomDistanceMatrix(N:Int):Array[Array[Int]]= Array.tabulate(N,N)((i,j) => if (i==0 ||j == 0) 0 else random.nextInt(1000)+1)

  def getPlanarDistanceMatrix(N:Int):Array[Array[Int]] = {
    val coordX = Array.tabulate(N)(_ => random.nextInt(1000))
    val coordY = Array.tabulate(N)(_ => random.nextInt(1000))
    Array.tabulate(N,N)((i,j) => round(sqrt((   pow(coordX(i) - coordX(j), 2)
                   + pow(coordY(i) - coordY(j), 2) ).toFloat)).toInt )
  }

  val DistanceMatrix = getPlanarDistanceMatrix(N)

  val m: Model = new Model(false,false,false,false)
  val vrp = new VRP(N, 1, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPoints with OptimizeThreeOptWithReverse
  vrp.installCostMatrix(DistanceMatrix)

  vrp.saveKNearestPoints(20)

  m.close()



  println("closed " + getWatchString)
  NearestNeighbor(vrp)
  //RandomNeighboor(vrp)
  m.propagate()

  println("start val: " + vrp.ObjectiveVar.value)
  println(vrp)
  println(vrp.routes)
  var nsize = 50
  // 20 for the ThreeOpt withtout reverse
  // 20-100 for the ThreeOpt with 1 or 2 reverse works well

  var Neighbor = false
  var move:Neighbor = null
  var saturated = false
  var it = 0
  while(!saturated){
    val oldobj:Int = vrp.ObjectiveVar.value
    //move = OnePointMove.getFirstImprovingMove(vrp,move)
   // move = ThreeOptMove.getFirstImprovingMove(vrp, nsize, move)
    move = ThreeOptTwoReverseMove.getFirstImprovingMove(vrp, nsize, move)
    //move = ThreeOptOneReverseMove.getFirstImprovingMove(vrp, nsize, move)
    if (move != null && move.getObjAfter < oldobj){
      it +=1
      move.comit
      //vrp.ObjectiveVar.value
      println("it: " + it + " " + move + " " + vrp.ObjectiveVar.value)

    }
    else{
        saturated = true
        println("done " + getWatchString)
        println("Nb Reverse = "+vrp.reverseNb)
        if(realSum(vrp,DistanceMatrix).equals(vrp.ObjectiveVar.value)) println("Youpie!!") else println("Ohhh :'(")
    }
  }




  println(vrp.ObjectiveVar.value)
}

