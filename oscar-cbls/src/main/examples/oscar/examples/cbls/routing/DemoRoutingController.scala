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
package oscar.examples.cbls.routing

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.lib.logic.Int2Int
import oscar.cbls.invariants.lib.numeric.{Abs, Sum}
import oscar.cbls.routing.model._
import oscar.cbls.routing.neighborhood._
import oscar.cbls.search.StopWatch
import oscar.cbls.modeling.Algebra._
import oscar.cbls.search.combinators.{BestSlopeFirst, Profile, RoundRobin}
import oscar.cbls.search.move.Move


class MyDemoVRP(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]], unroutedPenaltyWeight:Int, loadBalancing:Double = 1.0)
  extends VRP(n,v,model)
    with HopDistanceAsObjectiveTerm
    with HopClosestNeighbors
    with PositionInRouteAndRouteNr
    with NodesOfVehicle
    with PenaltyForUnrouted
    with hopDistancePerVehicle
    with Predecessors
    with PenaltyForEmptyRouteAsObjectiveTerm{ //just for the fun of it

  installCostMatrix(distanceMatrix)
  setUnroutedPenaltyWeight(unroutedPenaltyWeight)
  closeUnroutedPenaltyWeight()
  computeClosestNeighbors()
  println("end compute closest, install matrix")
  installHopDistancePerVehicle()
  println("end install matrix, posting constraints")

  //evenly spreading the travel among vehicles
  val averageDistanceOnAllVehicles = overallDistance / V
  val spread = Sum(hopDistancePerVehicle.map(h => Abs(h - averageDistanceOnAllVehicles)))
  val weightedSpread = new Int2Int(spread,(spreadValue:Int) => (spreadValue * loadBalancing).toInt)

  addObjectiveTerm(weightedSpread)
  addObjectiveTerm(unroutedPenalty)

  setEmptyRoutePenaltyWeight(100)
  println("vrp done")
}

class DemoRoutingController extends StopWatch{

  var customersNumber = 0
  var carsNumber = 0
  var myVRP:MyDemoVRP = null
  var model:Store = null

  def initiateProblem(customers:Int, cars:Int, s:Int,u:Int):List[(Int,Int)]={
    val generatedMatrix = RoutingMatrixGenerator(customers,s)
    customersNumber = customers
    carsNumber = cars
    model = new Store()
    myVRP = new MyDemoVRP(customers,cars,model,generatedMatrix._1,u)
    model.close()
    generatedMatrix._2.toList
  }

  def resetProblem = {
    customersNumber = 0
    carsNumber = 0
    myVRP = null
    model = null
  }

   def resolveProblem:Boolean = {
     if(myVRP == null) false
     println("closed model " + getWatch + "ms")
     startWatch()
     val insertPointRoutedFirst = Profile(InsertPointRoutedFirst(
       insertionPoints = myVRP.routed,
       unroutedNodesToInsert = () => myVRP.kNearest(10,!myVRP.isRouted(_)),
       vrp = myVRP) guard(() => myVRP.unrouted.value.nonEmpty))

     val insertPointUnroutedFirst = Profile(InsertPointUnroutedFirst(
       unroutedNodesToInsert= myVRP.unrouted,
       relevantNeighbors = () => myVRP.kNearest(10,myVRP.isRouted(_)),
       vrp = myVRP))

     val insertPointUnroutedFirstBest = Profile(InsertPointUnroutedFirst(
       unroutedNodesToInsert= myVRP.unrouted,
       relevantNeighbors = () => myVRP.kNearest(1,myVRP.isRouted(_)),
       neighborhoodName = "insertPointUnroutedFirstBest",
       vrp = myVRP, best = true))

     val pivot = myVRP.N/2

     val compositeInsertPoint = Profile((insertPointRoutedFirst guard(() => myVRP.unrouted.value.size >= pivot)
       orElse (insertPointUnroutedFirst guard(() => myVRP.unrouted.value.size < pivot))))

     //the other insertion point strategy is less efficient, need to investigate why.
     val insertPoint = compositeInsertPoint //insertPointUnroutedFirstBest //new BestSlopeFirst(List(insertPointUnroutedFirst,insertPointRoutedFirst),refresh = 50) //compositeInsertPoint //insertPointUnroutedFirst

     val onePointMove = Profile(OnePointMove(
       nodesPrecedingNodesToMove = myVRP.routed,
       relevantNeighbors= () => myVRP.kNearest(50),
       vrp = myVRP))

     val twoOpt = Profile(TwoOpt(
       predecesorOfFirstMovedPoint = myVRP.routed,
       relevantNeighbors = () => myVRP.kNearest(20),
       vrp = myVRP))

     val threeOpt = Profile(ThreeOpt(
       potentialInsertionPoints = myVRP.routed,
       relevantNeighbors = () => myVRP.kNearest(20),
       vrp = myVRP,
       skipOnePointMove = true))

     val segExchange = Profile(SegmentExchange(vrp = myVRP,
       relevantNeighbors = () => myVRP.kNearest(40),
       vehicles=() => myVRP.vehicles.toList))

     val search = new RoundRobin(scala.collection.immutable.List(insertPoint,onePointMove),10).afterMoveOnMove((m:Move) =>{
       val routesList:List[List[Int]] = (for(c <- 0 to carsNumber-1)yield myVRP.getRouteOfVehicle(c)).toList
       DemoRoutingView.drawMove(routesList,(myVRP.getObjective().value,getWatch, m.neighborhoodName), myVRP.hopDistancePerVehicle)
     }) exhaust
       (new BestSlopeFirst(List(onePointMove,threeOpt,segExchange),refresh = customersNumber/2)).afterMoveOnMove((m:Move) =>{
         val routesList:List[List[Int]] = (for(c <- 0 to carsNumber-1)yield myVRP.getRouteOfVehicle(c)).toList
         DemoRoutingView.drawMove(routesList,(myVRP.getObjective().value,getWatch, m.neighborhoodName), myVRP.hopDistancePerVehicle)
       })
     // exhaust onePointMove exhaust segExchange//threeOpt //(new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))

     search.verbose = 1
     //    search.verboseWithExtraInfo(3,() => myVRP.toString)
     //segExchange.verbose = 3
     search.doAllMoves(_ > 10*customersNumber, myVRP.objectiveFunction)

     println("total time " + getWatch + "ms or  " + getWatchString)

     println("\nresult:\n" + myVRP)

     println(search.profilingStatistics)
     DemoRoutingView.displayEndStatistics(Profile.statisticsHeader,search.collectProfilingStatistics)

     true
   }
}
