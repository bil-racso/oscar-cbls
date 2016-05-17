package oscar.examples.cbls.pdptw

import java.awt.Toolkit
import javax.swing.JFrame

import oscar.cbls.invariants.core.computation.{IntValue, Store}
import oscar.cbls.invariants.lib.numeric.{Abs, Sum}
import oscar.cbls.routing.model._
import oscar.cbls.routing.neighborhood._
import oscar.cbls.search.StopWatch
import oscar.cbls.search.combinators._
import oscar.cbls.search.move.CompositeMove
import oscar.examples.cbls.routing.RoutingMatrixGenerator

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

class MyPDPTW(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]], pointsList:Array[(Int,Int)], mapSize:Int,
              unroutedPenalty:Int, pickups:Array[Int] = null, deliverys:Array[Int] = null)
  extends PDP(n,v,model)
    with HopDistanceAsObjectiveTerm
    with HopClosestNeighbors
    with PositionInRouteAndRouteNr
    with NodesOfVehicle
    with PenaltyForUnrouted
    with PenaltyForUnroutedAsObjectiveTerm
    with PenaltyForEmptyRoute
    with PenaltyForEmptyRouteAsObjectiveTerm
    with hopDistancePerVehicle
    with VehicleWithCapacity
    with PositionInTime
    with TimeWindow
    with MaxTravelDistancePDConstraint
    with PDPMap{

  /**
    * This method set a linear travel time function. The time function is entirely based on the distanceMatrix
    * @param distanceMatrix the distance matrix of the map
    */
  def setLinearTravelTimeFunction(distanceMatrix: Array[Array[Int]]): Unit ={
    val ttf = new TTFMatrix(N,new TTFConst(500))
    for(i <- 0 until N){
      for(j <- 0 until N){
        ttf.setTTF(i,j,new TTFConst(distanceMatrix(i)(j)))
      }
    }
    setTravelTimeFunctions(ttf)
  }

  //TODO : Refactor this method, actually it's quite ugly and not very comprehensive
  /**
    *This method generate the time window for each point of the problem.
    * It ensures that the arrival time of a pickup node is before the arrival time of his related delivery node
    * And it tries to randomises the problem (with all the differents constraints, the problem is often unsolvable)
    */
  def endWindowGenerator(): Unit ={
    val currentArray:Array[Int] = new Array[Int](N-V)
    val randomIncValues:List[Int] = 2::3::4::5::Nil
    val currentSum = (pos:Int) => {
      var res = 0
      for(i <- 0 to pos) res += currentArray(i)
      res
    }

    val currentPickup:Array[Int] = new Array[Int](N-V)
    val currentSumPickup = (pos:Int) => {
      var res = 0
      for(i <- 0 to pos) res += currentPickup(i)
      res
    }
    var currentTimeUnit = 1
    val nodesOrderedByType = getPickups.toArray
    while(currentSum(currentTimeUnit) < N-V){
      val current = currentSum(currentTimeUnit)
      val currentPick = currentSumPickup(currentTimeUnit)
      val nbOfNodeToAdd = if(N - V - (N-V)/2 - currentPick < V) N-currentPick - (N-V)/2 -V else Math.min(N-V - (N-V)/2 -currentPick,Math.random()*(V*currentTimeUnit - current)).toInt
      for(inc <- 0 until nbOfNodeToAdd){
        val deliveryInc = randomIncValues(scala.util.Random.nextInt(4))
        setEndWindow(nodesOrderedByType(currentPick+inc), 500*(currentTimeUnit+1))
        setNodeDuration(nodesOrderedByType(currentPick+inc),50,500*currentTimeUnit)
        setEndWindow(getRelatedDelivery(nodesOrderedByType(currentPick+inc)),500*(currentTimeUnit+5))
        setNodeDuration(getRelatedDelivery(nodesOrderedByType(currentPick+inc)), 0, 500*(currentTimeUnit+deliveryInc))
        currentArray(currentTimeUnit+deliveryInc) += 1
      }
      currentArray(currentTimeUnit) += nbOfNodeToAdd
      currentPickup(currentTimeUnit) += nbOfNodeToAdd
      currentTimeUnit += 1
    }
  }

  installCostMatrix(distanceMatrix)
  setUnroutedPenaltyWeight(unroutedPenalty)
  setEmptyRoutePenaltyWeight(unroutedPenalty)
  closeUnroutedPenaltyWeight()
  computeClosestNeighbors()
  println("end compute closest, install matrix")
  installHopDistancePerVehicle()
  println("end install matrix, posting constraints")
  if(pickups == null && deliverys == null)
    addRandomPickupDeliveryCouples()
  else if(pickups != null && deliverys != null)
    addPickupDeliveryCouples(pickups,deliverys)
  else
    assert(false,"Either you specify the pickups and deliverys node or you specify nothing")
  setArrivalLeaveLoadValue()
  setVehiclesMaxCargo(4)
  setVehiclesCapacityStrongConstraint()
  setLinearTravelTimeFunction(distanceMatrix)
  endWindowGenerator()
  setMapInfo(pointsList,mapSize)
  //setMaxTravelDistancePDConstraint()

  //evenly spreading the travel among vehicles
  val averageDistanceOnAllVehicles = overallDistance.value / V
  val spread = Sum(hopDistancePerVehicle.map(h => Abs(h.value - averageDistanceOnAllVehicles)))
  addObjectiveTerm(spread)
  addObjectiveTerm(emptyRoutePenalty)

  println("vrp done")
}

object pdptwTest extends App with StopWatch {

  this.startWatch()

  val n = 65
  val v = 5

  println("PDPTWTest(n:" + n + " v:" + v + ")")

  val mapSize = 500

  val model = new Store(noCycle = false)

  //val (distanceMatrix,positions) = PickupDeliveryStarMatrixGenerator(n,v,mapSize)
  val (distanceMatrix,positions) = RoutingMatrixGenerator(n,mapSize)
  println("compozed matrix " + getWatch + "ms")

  val pdp = new MyPDPTW(n,v,model,distanceMatrix,positions,mapSize,mapSize,
    pickups = Array.tabulate((n-v)/2)(p => p+v), deliverys = Array.tabulate((n-v)/2)(d => d+v+((n-v)/2)))


  model.close()

  println("closed model " + getWatch + "ms")
  val insertPointRoutedFirst = Profile(InsertPointRoutedFirst(
    insertionPoints = pdp.routed,
    unroutedNodesToInsert = () => pdp.kNearest(10,!pdp.isRouted(_)),
    vrp = pdp) guard(() => pdp.unrouted.value.nonEmpty))

  val insertPointUnroutedFirst = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert= pdp.unrouted,
    relevantNeighbors = () => pdp.kNearest(10,pdp.isRouted(_)),
    vrp = pdp))

  val insertPointUnroutedFirstBest = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert= pdp.unrouted,
    relevantNeighbors = () => pdp.kNearest(1,pdp.isRouted(_)),
    neighborhoodName = "insertPointUnroutedFirstBest",
    vrp = pdp, best = true))

  val pivot = pdp.N/2

  val compositeInsertPoint = Profile(insertPointRoutedFirst guard (() => pdp.unrouted.value.size >= pivot)
    orElse (insertPointUnroutedFirst guard (() => pdp.unrouted.value.size < pivot)))

  //the other insertion point strategy is less efficient, need to investigate why.
  val insertPoint = compositeInsertPoint //insertPointUnroutedFirstBest //new BestSlopeFirst(List(insertPointUnroutedFirst,insertPointRoutedFirst),refresh = 50) //compositeInsertPoint //insertPointUnroutedFirst

  val onePointMove = Profile(OnePointMove(
    nodesPrecedingNodesToMove = pdp.routed,
    relevantNeighbors= () => pdp.kNearest(50),
    vrp = pdp))

  val twoOpt = Profile(TwoOpt(
    predecesorOfFirstMovedPoint = pdp.routed,
    relevantNeighbors = () => pdp.kNearest(20),
    vrp = pdp))

  val threeOpt = Profile(ThreeOpt(
    potentialInsertionPoints = pdp.routed,
    relevantNeighbors = () => pdp.kNearest(20),
    vrp = pdp,
    skipOnePointMove = true))

  val segExchange = Profile(SegmentExchange(vrp = pdp,
    relevantNeighbors = () => pdp.kNearest(40),
    vehicles=() => pdp.vehicles.toList))

  val insertCouple = Profile(DynAndThen(
    InsertPointUnroutedFirst(
    unroutedNodesToInsert = () => pdp.getUnroutedPickups,
    relevantNeighbors = () => pdp.getRoutedNodesBeforeTime(),
    vrp = pdp),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
    unroutedNodesToInsert = () => Iterable(pdp.getRelatedDelivery(moveResult.insertedPoint)),
    relevantNeighbors = () => pdp.kNearest(n,pdp.onTheSameRouteMultArg(moveResult.insertedPoint)),
    vrp = pdp, best = true))name "insertCouple")

  val oneCoupleMove = Profile(DynAndThen(OnePointMove(
    nodesPrecedingNodesToMove = () => pdp.getRoutedPickupsPredecessors,
    relevantNeighbors= () => pdp.kNearest(1000,pdp.isRouted),
    vrp = pdp),
    (moveResult:OnePointMoveMove) => OnePointMove(
    nodesPrecedingNodesToMove = () => List(pdp.preds(pdp.getRelatedDelivery(moveResult.movedPoint)).value),
    relevantNeighbors= () => pdp.kNearest(1000,pdp.onTheSameRouteMultArg(moveResult.movedPoint)),
    vrp = pdp, best = true))name "oneCoupleMove")

  val onePointMovePD = Profile(new RoundRobin(List(OnePointMove(
    nodesPrecedingNodesToMove = () => pdp.getRoutedPickupsPredecessors,
    relevantNeighbors = () => pdp.getNodesBeforeRelatedDelivery(),
    vrp = pdp,best = true),OnePointMove(
    nodesPrecedingNodesToMove = () => pdp.getRoutedDeliverysPredecessors,
    relevantNeighbors = () => pdp.getNodesBeforeRelatedPickup(),
    vrp = pdp, best = true)))name "OnePointMove PickupDelivery")

  val segExchangePD = Profile(SegmentExchangePickupAndDelivery(vrp = pdp))

  val orOpt = Profile(OrOpt(vrp = pdp))

  //Working but find nothing
  //val pickupDeliveryCoupleExchange = Profile(PickupDeliveryCoupleExchange(vrp = pdp))

  val dynAndThenCoupleExchange = Profile(
    new DynAndThen(
      //We first remove a PD couple from a first route
      new DynAndThen(new RemovePoint(
        predecessorsOfRoutedPointsToRemove = () => pdp.getRoutedPickupsPredecessors,
        vrp = pdp),
        (moveResult1:RemovePointMove) =>
        new RemovePoint(
          predecessorsOfRoutedPointsToRemove = () => List(pdp.preds(pdp.getRelatedDelivery(moveResult1.removedPoint)).value),
          vrp = pdp)
      )
      ,(moveResult2:CompositeMove) =>
        //Then we move a PD couple from a second route to the first route
        new DynAndThen(
          new DynAndThen(OnePointMove(
            nodesPrecedingNodesToMove = () =>
              pdp.getRoutedPickupsPredecessors.filter(pdp.routeNr(_).value != pdp.routeNr(moveResult2.ml.head.asInstanceOf[RemovePointMove].beforeRemovedPoint).value),
            relevantNeighbors= () => pdp.getRoutedNodesBeforeTimeOfRoute(List(pdp.routeNr(moveResult2.ml.head.asInstanceOf[RemovePointMove].beforeRemovedPoint).value)),
            vrp = pdp), (moveResult3:OnePointMoveMove) =>
            OnePointMove(
              nodesPrecedingNodesToMove = () => List(pdp.preds(pdp.getRelatedDelivery(moveResult3.movedPoint)).value),
              relevantNeighbors= () => pdp.kNearest(1000,pdp.onTheSameRouteMultArg(moveResult3.movedPoint)),
              vrp = pdp, best = true)
          )
          ,(moveResult3:CompositeMove) =>
            //And finally we insert the first couple in the second route
            new DynAndThen(InsertPointUnroutedFirst(
              unroutedNodesToInsert = () => Iterable(moveResult2.ml.head.asInstanceOf[RemovePointMove].removedPoint),
              relevantNeighbors = () => pdp.getRoutedNodesBeforeTimeOfRoute(List(pdp.routeNr(moveResult3.ml.head.asInstanceOf[OnePointMoveMove].predOfMovedPoint).value)),
              vrp = pdp), (moveResult5:InsertPointMove) =>
              InsertPointUnroutedFirst(
                unroutedNodesToInsert = () => Iterable(moveResult2.ml.apply(1).asInstanceOf[RemovePointMove].removedPoint),
                relevantNeighbors = () => pdp.kNearest(n,pdp.onTheSameRouteMultArg(moveResult5.insertedPoint)),
                vrp = pdp, best = true)
            )
          , maximalIntermediaryDegradation = Int.MaxValue/2
        )
      )name "dynAndThenCoupleExchange"
  )

  val pickupDeliveryCoupleShift = Profile(DynAndThen(OnePointMove(
    nodesPrecedingNodesToMove = () => pdp.getRoutedPickupsPredecessors,
    relevantNeighbors = () => pdp.getRoutedNodesBeforeTimeNotSameRoute(),
    vrp = pdp),
    (moveResult:OnePointMoveMove) => OnePointMove(
      nodesPrecedingNodesToMove = () => List(pdp.preds(pdp.getRelatedDelivery(moveResult.movedPoint)).value),
      relevantNeighbors = () => pdp.kNearest(n,pdp.onTheSameRouteMultArg(moveResult.movedPoint)),
      vrp = pdp, best = true))name "pickupDeliveryCoupleShift")

  var simulatedAnnealingValue = 1
  val simulatedAnnealingValueImpr = 2
  val liLimBaseLoop = new AndThen(
    new Best(
      new Retry(pickupDeliveryCoupleShift),
      new Retry(dynAndThenCoupleExchange)
    ),
    new Retry(onePointMovePD)
  )
  /*
  val liLimMetaHeuristique = insertCouple exhaust new MaxMovesWithoutImprovement(
    new BasicSaveBest(
      new AndThen(
        liLimBaseLoop,
        new MaxMovesWithoutImprovement(
          new AndThen(
            new WithAcceptanceCriterion(
              new Random(
                pickupDeliveryCoupleShift,
                dynAndThenCoupleExchange
              ),
              (a:Int,b:Int) => {if(b < a) true else if(Math.random() < Math.pow(Math.E,-(b-a)/simulatedAnnealingValue)) true else false}
            )afterMove(simulatedAnnealingValue *= simulatedAnnealingValueImpr),
            liLimBaseLoop
          ),
          null,
          5,
          () => vrp.getObjective().value
        )
      ),
      vrp.getObjective()
    ),
    null,
    5,
    () => vrp.getObjective().value
  ) showObjectiveFunction vrp.getObjective() afterMove rm.drawRoutes()
  liLimMetaHeuristique.verbose = 1*/


  val search = BestSlopeFirst(List(insertCouple, onePointMovePD)) exhaust
    BestSlopeFirst(List(insertCouple, dynAndThenCoupleExchange, threeOpt, orOpt, segExchangePD, oneCoupleMove, pickupDeliveryCoupleShift), refresh = n / 2) showObjectiveFunction
    pdp.getObjective() afterMove {
    pdp.drawRoutes()
  } // exhaust onePointMove exhaust segExchange//threeOpt //(new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))
  search.verbose = 1

  //pickupDeliveryCoupleExchange.verboseWithExtraInfo(3,() => vrp.routes.next.toList.toString)
  //search.paddingLength = 400
  //insertCouple.verbose = 3
  //    search.verboseWithExtraInfo(3,() => vrp.toString)
  //segExchange.verbose = 3

  def launchSearch(): Unit ={
    //search.verboseWithExtraInfo(1,vrp.toString)
    search.doAllMoves(_ > 10*n, pdp.getObjective())

    println("total time " + getWatch + "ms or  " + getWatchString)

    println("\nresult:\n" + pdp)

    println(search.profilingStatistics)
  }

  launchSearch()
}
