package oscar.examples.cbls.routing

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.objective.Objective
import oscar.cbls.routing.model._
import oscar.cbls.routing.neighborhood._
import oscar.cbls.search.StopWatch
import oscar.cbls.search.combinators.BestSlopeFirst

/**
 * Created by rdl on 15-12-15.
 */
object RoutingTest extends App with StopWatch{

  this.startWatch()

  val n = 1000
  val v = 10

  println("RoutingTest(n:" + n + " v:" + v + ")")

  val (distanceMatrix,positions) = RoutingMatrixGenerator(n,100)

  println("compozed matrix " + getWatch + "ms")

  val model = new Store()
  val vrp = new VRP(n,v,model)
    with HopDistanceAsObjectiveTerm
    with HopClosestNeighbors
    with PositionInRouteAndRouteNr
    with NodesOfVehicle
    with PenaltyForUnroutedAsObjectiveTerm

  vrp.installCostMatrix(distanceMatrix)
  vrp.setUnroutedPenaltyWeight(10000)
  vrp.computeClosestNeighbors()

  model.close()
  println("closed model " + getWatch + "ms")
  val insertPointRoutedFirst = new InsertPointRoutedFirst(
    insertionPoints = vrp.routed,
    unroutedNodesToInsert = () => vrp.kNearest(10,!vrp.isRouted(_)),
    vrp = vrp)

  val insertPointUnroutedFirst = new InsertPointUnroutedFirst(
    unroutedNodesToInsert= vrp.unrouted,
    relevantNeighbors = () => vrp.kNearest(20,vrp.isRouted(_)),
    vrp = vrp)

  //the other insertion point strategy is less efficient, need to investigate why.
  val insertPoint = insertPointUnroutedFirst

  val onePointMove = new OnePointMove(
    nodesPrecedingNodesToMove = vrp.routed,
    relevantNeighbors= () => vrp.kNearest(20),
    vrp = vrp)

  val twoOpt = TwoOpt(
    predecesorOfFirstMovedPoint = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp)

  val threeOpt = ThreeOpt(
    potentialInsertionPoints = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp)

  val search = insertPoint exhaustBack (new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))

  search.verbose = 1
//  search.verboseWithExtraInfo(3,() => vrp.toString)

  search.doAllMoves(_ > 10*n, vrp.objectiveFunction)

  println("total time " + getWatch + "ms or  " + getWatchString)

  println("\nresult:\n" + vrp)
}
