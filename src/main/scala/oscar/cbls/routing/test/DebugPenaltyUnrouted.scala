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
import math._
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.routing._
import heuristic.NearestNeighbor

/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 26/10/12
 * Time: 17:39
 * To change this template use File | Settings | File Templates.
 */


object DebugPenaltyUnrouted extends App{

  def getPlanarDistanceMatrix(coordX:Array[Int],coordY:Array[Int]):Array[Array[Int]] = {
    val N = coordX.length
    Array.tabulate(N,N)((i,j) => round(sqrt((   pow(coordX(i) - coordX(j), 2)
      + pow(coordY(i) - coordY(j), 2) ).toFloat)).toInt)
  }

  val V :Int = 1
  val N :Int = 8

  val matrix = getPlanarDistanceMatrix(Array(0,1,2,3,4,5,6,7,8),Array(0,0,0,0,0,0,0,0,0))
  val m: Model = new Model(false,false,false,false)
  val vrp= new VRP(N, 1, m) with HopDistanceAndOtherAsObjective with PositionInRouteAndRouteNr with PenaltyForUnrouted

  vrp.installCostMatrix(matrix)
  //vrp.fixPenaltyWeight(100)
  vrp.fixPenaltyWeight(2,100)
  vrp.recordAddedFunction(vrp.Penalty)
  m.close()

  NearestNeighbor(vrp)
  m.propagate()

  println(vrp)
  println(vrp.routes)
  println("Objective = "+vrp.ObjectiveVar)
  println("Pénalité = " +vrp.Penalty.value)

  // unroute one point
  vrp.Next(1) := 3
  vrp.Next(2) := N
  m.propagate()

  println(vrp)
  println(vrp.routes)
  println("Objective = "+vrp.ObjectiveVar)
  println("Pénalité = "+vrp.Penalty.value)

}
