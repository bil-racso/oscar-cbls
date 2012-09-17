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

/**supports only a single vehicle*/
object tspsolver extends SearchEngine with StopWatch with App{

  val N:Int = 1000

  this.startWatch()
  
  println("TSP(" + N + ")")
  val random = new Random(0)
  
  def getRandomDistanceMatrix(N:Int):Array[Array[Int]]= Array.tabulate(N,N)((i,j) => if (i==0 ||j == 0) 0 else random.nextInt(1000)+1)
  def getPlanarDistanceMatrix(N:Int):Array[Array[Int]] = {
    val coordX = Array.tabulate(N)(_ => random.nextInt(1000))
    val coordY = Array.tabulate(N)(_ => random.nextInt(1000))
    Array.tabulate(N,N)((i,j) => if (i==0 ||j == 0) 0 else
      math.round(math.sqrt((   math.pow(coordX(i) - coordX(j), 2)
                             + math.pow(coordY(i) - coordY(j), 2) ).toFloat)).toInt)
  }

  val DistanceMatrix = getPlanarDistanceMatrix(N)

  val m: Model = new Model(false,false,false,false)
  val vrp = new VRP(N, 1, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr
  vrp.installCostMatrix(DistanceMatrix)

  m.close()

  println("closed " + getWatchString)

  NearestNeighbor(vrp)
  m.propagate()

  println("start val: " + vrp.objective)

  var saturated = false
  var move:OnePointMove = null
  var it = 0
  while(!saturated){
    val oldobj:Int = vrp.objective.value
    move = OnePointMove.getFirstImprovingMove(vrp,move)
    if (move != null && move.getObjAfter < oldobj){
      vrp.objective.value
      println("it: " + it + " " + move + " " + vrp.objective)
      it +=1
      move.comit
    }else saturated = true
  }

  println("done " + getWatchString)

  println(vrp)
}

