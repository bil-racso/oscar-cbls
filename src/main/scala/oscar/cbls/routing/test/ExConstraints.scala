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

package oscar.cbls.routing.test

import data.{Point, BelgiumInstance}
import math._
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.basic.LE
import oscar.cbls.invariants.core.computation.{IntVar, Model}
import oscar.cbls.invariants.lib.logic.Cluster
import oscar.cbls.invariants.lib.numeric.SumElements
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.routing._
import initialSolution.RandomNeighbor
import model._

object ExConstraints extends App{

  val N = 20
  val vehicles = 2
  val kLimited = 20

  val m: Model = new Model(false,None,false,false)
  val vrp = new VRP(N, vehicles, m) with HopDistanceAsObjective with PositionInRouteAndRouteNr with ClosestNeighborPoints
    with PenaltyForUnrouted with OtherFunctionToObjective with WeightedNode with WeakConstraints with StrongConstraints

  def distanceMatrix(towns : Array[Point]):Array[Array[Int]] =
    Array.tabulate(N,N)((i,j) => round(sqrt((pow(towns(i).long - towns(j).long, 2)
      + pow(towns(i).lat - towns(j).lat, 2) ).toFloat)).toInt )

  vrp.installCostMatrix(distanceMatrix(BelgiumInstance.random(N)))
  vrp.saveKNearestPoints(kLimited)


  val strongConstraintSystem = new ConstraintSystem(m)
  val weakConstraintSystem = new ConstraintSystem(m)

  /*
    EXAMPLE 1:
    ----------
    Example of constraints based on unrouted node.

   */
  // fix node penalty
  vrp.fixUnroutedPenaltyWeight(100)
  //vrp.fixUnroutedPenaltyWeight(2,222) //specific weight for a given node
  val nbOfAllowedUnroutedNode = new IntVar(m,0,N,10,"nb of allowed unrouted node")
  // post and register the constraint
  weakConstraintSystem.post(LE(Cardinality(vrp.Unrouted),nbOfAllowedUnroutedNode),vrp.UnroutedPenalty)
  weakConstraintSystem.violation(vrp.Unrouted)
  // could be also strongConstraint if needed.

  /*
  * Or simply add the weight of an unrouted node to the objective.
  */

  vrp.recordAddedFunction(vrp.UnroutedPenalty)


  /*
    EXAMPLE 2:
    ----------
    Example of constraints based on route length. (max length in this example, easy to adapt for a min length too)

   */
  // fix a penalty (use node weight, or a penalty fixed by route or for all route, or anything else) and a length max.

  val lengthMax = new IntVar(m,0,N,50,"length max for route")
  // post and register the constraint
  for(i <- 0 until vrp.V){
    strongConstraintSystem.post(LE(vrp.RouteLength(i),lengthMax))
    strongConstraintSystem.violation(vrp.RouteLength(i))
    // once more it could be a weakConstraint, it depends only of the problem's definition.
  }

  /*
    EXAMPLE 3:
    ----------
    Example of constraints based on capacity of vehicle. (CVRP)

   */

  val cluster = Cluster.MakeDense(vrp.RouteNr)
  val capacityOfRoute = new IntVar(m,0,Int.MaxValue,100,"Capacity of vehicle")
  vrp.fixWeightNode(50)


  for (i <- 0 until vrp.V){
    val actualCapacityOfRoute = SumElements(vrp.weightNode,cluster.clusters(i)).toIntVar
    strongConstraintSystem.post(LE(actualCapacityOfRoute,capacityOfRoute))
    //strongConstraintSystem.registerForViolation(actualCapacityOfRoute)
  }

  vrp.setStrongConstraints(strongConstraintSystem)
  vrp.setWeakConstraints(weakConstraintSystem)
  strongConstraintSystem.close()
  weakConstraintSystem.close()
  m.close()

  RandomNeighbor(vrp)
  m.propagate()

}