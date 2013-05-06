/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import math._
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.routing.model._
import oscar.cbls.routing.initialSolution.NearestNeighbor
import scala.language.reflectiveCalls

class TestUnroutedAndPenalty extends FunSuite with ShouldMatchers{

  def fixture =
    new {
      /**
       * Gives a distance matrix by entering the abscissa and
       * ordinates of points in the plan.
       */
      def getDistanceMatrix(abscissa:Array[Int],ordinate:Array[Int]):Array[Array[Int]] = {
        val N = abscissa.length
        Array.tabulate(N,N)((i,j) => round(sqrt((   pow(abscissa(i) - abscissa(j), 2)
          + pow(ordinate(i) - ordinate(j), 2) ).toFloat)).toInt)
      }
      val UNROUTED = 9
      val ROUTE_ARRAY_UNROUTED = 1
      val V:Int = 1
      val N:Int = 9

      val matrix = getDistanceMatrix(Array(0,1,2,3,4,5,6,7,8),Array(0,0,0,0,0,0,0,0,0))
      val model: Model = new Model(false,None,false,false)

      val vrp = new VRP(N, V, model) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPoints
        with PenaltyForUnrouted
      vrp.installCostMatrix(matrix)
      model.close()
      NearestNeighbor(vrp)
      // 0 -> 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8 (-> 0)
      model.propagate()
    }

    test("instantiation"){
      val f = fixture
      f.vrp.UnroutedPenalty.value should be(0)
      f.vrp.weightUnroutedPenalty.foreach {v => v.value should be(0)}
    }

    test("fixe a penalty of 100 on node 1"){
      val f = fixture
      f.vrp.fixUnroutedPenaltyWeight(1,100)
      f.vrp.weightUnroutedPenalty(1).value should be(100)
      f.vrp.UnroutedPenalty.value should be(0)
    }

    test("fixe a penalty of 1000 on each node"){
      val f = fixture
      f.vrp.fixUnroutedPenaltyWeight(1000)
      f.vrp.weightUnroutedPenalty.foreach {v => v.value should be(1000)}
      f.vrp.UnroutedPenalty.value should be(0)
    }

    test("fixe a penalty of 100 on node 1 and unroute 1"){
      val f = fixture
      f.vrp.fixUnroutedPenaltyWeight(1,100)
      f.vrp.weightUnroutedPenalty(1).value should be(100)
      f.vrp.UnroutedPenalty.value should be(0)

      f.vrp.remove(List((0,1))).foreach(t => t._1 := t._2)
      f.model.propagate()

      f.vrp.UnroutedPenalty.value should be(100)
    }

  test("fixe a penalty of 100 on 3 nodes, unroute them, then reinsert them"){
    val f = fixture
    f.vrp.fixUnroutedPenaltyWeight(1,100)
    f.vrp.fixUnroutedPenaltyWeight(4,100)
    f.vrp.fixUnroutedPenaltyWeight(5,100)

    f.vrp.weightUnroutedPenalty(1).value should be(100)
    f.vrp.weightUnroutedPenalty(4).value should be(100)
    f.vrp.weightUnroutedPenalty(5).value should be(100)
    f.vrp.UnroutedPenalty.value should be(0)

    f.vrp.remove(List((0,1),(3,5))).foreach(t => t._1 := t._2)
    f.model.propagate()

    f.vrp.UnroutedPenalty.value should be(300)

    f.vrp.add(0,1).foreach(t => t._1 := t._2)
    f.model.propagate()
    f.vrp.UnroutedPenalty.value should be(200)

    f.vrp.add(0,4).foreach(t => t._1 := t._2)
    f.model.propagate()
    f.vrp.UnroutedPenalty.value should be(100)

    f.vrp.add(0,5).foreach(t => t._1 := t._2)
    f.model.propagate()
    f.vrp.UnroutedPenalty.value should be(0)
  }

}
