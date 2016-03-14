package oscar.examples.cbls.pdptw

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.lib.numeric.{Abs, Sum}
import oscar.cbls.routing.model._

/**
  * *****************************************************************************
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
  * ****************************************************************************
  */

/**
  * @author fabian.germeau@student.vinci.be
  */

class MyVRP(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]],unroutedPenalty:Int)
  extends VRP(n,v,model)
    with HopDistanceAsObjectiveTerm
    with HopClosestNeighbors
    with PositionInRouteAndRouteNr
    with NodesOfVehicle
    with PenaltyForUnrouted
    with hopDistancePerVehicle
    with VehicleWithCapacity{

  installCostMatrix(distanceMatrix)
  setUnroutedPenaltyWeight(unroutedPenalty)
  closeUnroutedPenaltyWeight()
  computeClosestNeighbors()
  println("end compute closest, install matrix")
  installHopDistancePerVehicle()
  println("end install matrix, posting constraints")
  setVehiclesMaxCargo(4)
  setVehiclesCargo(0)

  //evenly spreading the travel among vehicles
  val averageDistanceOnAllVehicles = overallDistance / V
  val spread = Sum(hopDistancePerVehicle.map(h => Abs(h - averageDistanceOnAllVehicles)))
  addObjectiveTerm(spread)
  addObjectiveTerm(unroutedPenalty)

  println("vrp done")
}

object pdptwTest {

}
