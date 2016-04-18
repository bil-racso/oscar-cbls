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
import oscar.examples.cbls.routing.visual.ColorGenerator
import oscar.examples.cbls.routing.visual.MatrixMap.{PickupAndDeliveryMatrixVisualWithAttribute, PickupAndDeliveryPoints, RoutingMatrixVisualWithAttribute, RoutingMatrixVisual}
import oscar.visual.VisualFrame

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

class MyPDPTWVRP(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]],unroutedPenalty:Int)
  extends VRP(n,v,model)
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
    with PickupAndDeliveryCustomersWithTimeWindow
    with MaxTravelDistancePDConstraint{

  installCostMatrix(distanceMatrix)
  setUnroutedPenaltyWeight(unroutedPenalty)
  setEmptyRoutePenaltyWeight(unroutedPenalty)
  closeUnroutedPenaltyWeight()
  computeClosestNeighbors()
  println("end compute closest, install matrix")
  installHopDistancePerVehicle()
  println("end install matrix, posting constraints")
  addRandomPickupDeliveryCouples()
  setArrivalLeaveLoadValue()
  setVehiclesMaxCargo(4)
  setVehiclesCapacityStrongConstraint()
  setLinearTravelTimeFunction(distanceMatrix)
  endWindowGenerator()
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

  val (distanceMatrix,positions) = RoutingMatrixGenerator(n,mapSize)
  println("compozed matrix " + getWatch + "ms")


  val model = new Store(noCycle = false)

  val vrp = new MyPDPTWVRP(n,v,model,distanceMatrix,mapSize)

  model.close()


  val f = new JFrame("PDPTWTest - Routing Map")
  f.setSize(Toolkit.getDefaultToolkit.getScreenSize.getWidth.toInt,(11*Toolkit.getDefaultToolkit().getScreenSize().getHeight/12).toInt)
  val rm = new PickupAndDeliveryMatrixVisualWithAttribute("Routing Map",vrp,mapSize,positions.toList,ColorGenerator.generateRandomColors(v),dimension = f.getSize)
  f.add(rm)
  f.pack()
  f.setVisible(true)

  println("closed model " + getWatch + "ms")
  val insertPointRoutedFirst = Profile(InsertPointRoutedFirst(
    insertionPoints = vrp.routed,
    unroutedNodesToInsert = () => vrp.kNearest(10,!vrp.isRouted(_)),
    vrp = vrp) guard(() => vrp.unrouted.value.nonEmpty))

  val insertPointUnroutedFirst = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert= vrp.unrouted,
    relevantNeighbors = () => vrp.kNearest(10,vrp.isRouted(_)),
    vrp = vrp))

  val insertPointUnroutedFirstBest = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert= vrp.unrouted,
    relevantNeighbors = () => vrp.kNearest(1,vrp.isRouted(_)),
    neighborhoodName = "insertPointUnroutedFirstBest",
    vrp = vrp, best = true))

  val pivot = vrp.N/2

  val compositeInsertPoint = Profile(insertPointRoutedFirst guard (() => vrp.unrouted.value.size >= pivot)
    orElse (insertPointUnroutedFirst guard (() => vrp.unrouted.value.size < pivot)))

  //the other insertion point strategy is less efficient, need to investigate why.
  val insertPoint = compositeInsertPoint //insertPointUnroutedFirstBest //new BestSlopeFirst(List(insertPointUnroutedFirst,insertPointRoutedFirst),refresh = 50) //compositeInsertPoint //insertPointUnroutedFirst

  val onePointMove = Profile(OnePointMove(
    nodesPrecedingNodesToMove = vrp.routed,
    relevantNeighbors= () => vrp.kNearest(50),
    vrp = vrp))

  val twoOpt = Profile(TwoOpt(
    predecesorOfFirstMovedPoint = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp))

  val threeOpt = Profile(ThreeOpt(
    potentialInsertionPoints = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp,
    skipOnePointMove = true))

  val segExchange = Profile(SegmentExchange(vrp = vrp,
    relevantNeighbors = () => vrp.kNearest(40),
    vehicles=() => vrp.vehicles.toList))

  val insertCouple = Profile(DynAndThen(
    InsertPointUnroutedFirst(
    unroutedNodesToInsert = () => vrp.getUnroutedPickups,
    relevantNeighbors = () => vrp.getRoutedNodesBeforeTime(),
    vrp = vrp),
    (moveResult:InsertPointMove) => InsertPointUnroutedFirst(
    unroutedNodesToInsert = () => Iterable(vrp.getRelatedDelivery(moveResult.insertedPoint)),
    relevantNeighbors = () => vrp.kNearest(n,vrp.onTheSameRouteMultArg(moveResult.insertedPoint)),
    vrp = vrp, best = true)))name "insertCouple"

  val oneCoupleMove = Profile(DynAndThen(OnePointMove(
    nodesPrecedingNodesToMove = () => vrp.getRoutedPickupsPredecessors,
    relevantNeighbors= () => vrp.kNearest(1000,vrp.isRouted),
    vrp = vrp),
    (moveResult:OnePointMoveMove) => OnePointMove(
    nodesPrecedingNodesToMove = () => List(vrp.preds(vrp.getRelatedDelivery(moveResult.movedPoint)).value),
    relevantNeighbors= () => vrp.kNearest(1000,vrp.onTheSameRouteMultArg(moveResult.movedPoint)),
    vrp = vrp, best = true)))name "oneCoupleMove"

  val onePointMovePD = Profile(new RoundRobin(List(OnePointMove(
    nodesPrecedingNodesToMove = () => vrp.getRoutedPickupsPredecessors,
    relevantNeighbors = () => vrp.getAuthorizedInsertionPositionForPickup(),
    vrp = vrp,best = true),OnePointMove(
    nodesPrecedingNodesToMove = () => vrp.getRoutedDeliverysPredecessors,
    relevantNeighbors = () => vrp.getAuthorizedInsertionPositionForDelivery(),
    vrp = vrp, best = true))))name "OnePointMove PickupDelivery"

  val segExchangePD = Profile(SegmentExchangePickupAndDelivery(vrp = vrp))

  val orOpt = Profile(OrOpt(vrp = vrp))

  val pickupDeliveryCoupleExchange = Profile(PickupDeliveryCoupleExchange(vrp = vrp))
  val dynAndThenCoupleExchange = Profile(
    new DynAndThen(
      //We first remove a PD couple from a first route
      new DynAndThen(new RemovePoint(
        predecessorsOfRoutedPointsToRemove = () => vrp.getRoutedPickupsPredecessors,
        vrp = vrp),
        (moveResult1:RemovePointMove) =>
        new RemovePoint(
          predecessorsOfRoutedPointsToRemove = () => List(vrp.preds(vrp.getRelatedDelivery(moveResult1.removedPoint)).value),
          vrp = vrp)
      )
      ,(moveResult2:CompositeMove) =>
        //Then we move a PD couple from a second route to the first route
        new DynAndThen(
          new DynAndThen(OnePointMove(
            nodesPrecedingNodesToMove = () =>
              vrp.getRoutedPickupsPredecessors.filter(vrp.routeNr(_).value != vrp.routeNr(moveResult2.ml.head.asInstanceOf[RemovePointMove].beforeRemovedPoint).value),
            relevantNeighbors= () => vrp.getRoutedNodesBeforeTimeOfRoute(List(vrp.routeNr(moveResult2.ml.head.asInstanceOf[RemovePointMove].beforeRemovedPoint).value)),
            vrp = vrp),(moveResult3:OnePointMoveMove) =>
            OnePointMove(
              nodesPrecedingNodesToMove = () => List(vrp.preds(vrp.getRelatedDelivery(moveResult3.movedPoint)).value),
              relevantNeighbors= () => vrp.kNearest(1000,vrp.onTheSameRouteMultArg(moveResult3.movedPoint)),
              vrp = vrp, best = true)
          )
          ,(moveResult3:CompositeMove) =>
            //And finally we insert the first couple in the second route
            new DynAndThen(InsertPointUnroutedFirst(
              unroutedNodesToInsert = () => Iterable(moveResult2.ml.head.asInstanceOf[RemovePointMove].removedPoint),
              relevantNeighbors = () => vrp.getRoutedNodesBeforeTimeOfRoute(List(vrp.routeNr(moveResult3.ml.head.asInstanceOf[OnePointMoveMove].predOfMovedPoint).value)),
              vrp = vrp),(moveResult5:InsertPointMove) =>
              InsertPointUnroutedFirst(
                unroutedNodesToInsert = () => Iterable(moveResult2.ml.apply(1).asInstanceOf[RemovePointMove].removedPoint),
                relevantNeighbors = () => vrp.kNearest(n,vrp.onTheSameRouteMultArg(moveResult5.insertedPoint)),
                vrp = vrp, best = true)
            )
          , maximalIntermediaryDegradation = Int.MaxValue/2
        )
      )
  )name "dynAndThenCoupleExchange"

  val pickupDeliveryCoupleShift = Profile(DynAndThen(OnePointMove(
    nodesPrecedingNodesToMove = () => vrp.getRoutedPickupsPredecessors,
    relevantNeighbors = () => vrp.getRoutedNodesBeforeTimeNotSameRoute(),
    vrp = vrp),
    (moveResult:OnePointMoveMove) => OnePointMove(
      nodesPrecedingNodesToMove = () => List(vrp.preds(vrp.getRelatedDelivery(moveResult.movedPoint)).value),
      relevantNeighbors = () => vrp.kNearest(n,vrp.onTheSameRouteMultArg(moveResult.movedPoint)),
      vrp = vrp, best = true)))name "pickupDeliveryCoupleShift"

  //TODO : Take the best value of the used neighborhoods(need to fix dynAndThen first)
  var simulatedAnnealingValue = 1
  val simulatedAnnealingValueImpr = 2
  val liLimBaseLoop = new AndThen(
    new Best(
      new Retry(pickupDeliveryCoupleShift),
      new Retry(dynAndThenCoupleExchange)
    ),
    new Retry(onePointMovePD)
  )
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
  liLimMetaHeuristique.verbose = 1


  /*val search = BestSlopeFirst(List(insertCouple, onePointMovePD)) exhaust
    BestSlopeFirst(List(insertCouple, dynAndThenCoupleExchange, onePointMovePD, threeOpt, orOpt, segExchangePD, oneCoupleMove), refresh = n / 2) showObjectiveFunction
    vrp.getObjective() afterMove {
    rm.drawRoutes()
  } // exhaust onePointMove exhaust segExchange//threeOpt //(new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))
  search.verbose = 1*/

  //pickupDeliveryCoupleExchange.verboseWithExtraInfo(3,() => vrp.routes.next.toList.toString)
  //search.paddingLength = 400
  //insertCouple.verbose = 3
  //    search.verboseWithExtraInfo(3,() => vrp.toString)
  //segExchange.verbose = 3

  def launchSearch(): Unit ={
    //search.verboseWithExtraInfo(1,vrp.toString)
    liLimMetaHeuristique.doAllMoves(_ > 10*n, vrp.getObjective())

    println("total time " + getWatch + "ms or  " + getWatchString)

    println("\nresult:\n" + vrp)

    println(liLimMetaHeuristique.profilingStatistics)
  }

  launchSearch()
}
