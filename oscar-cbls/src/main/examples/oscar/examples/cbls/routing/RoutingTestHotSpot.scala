package oscar.examples.cbls.routing

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.routing.model._
import oscar.cbls.routing.neighborhood._
import oscar.cbls.search.{Benchmark, StopWatch}
import oscar.cbls.search.combinators.{Profile, BestSlopeFirst}

class MyVRP(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]],unroutedPenalty:Int)
  extends VRP(n,v,model)
  with HopDistanceAsObjectiveTerm
  with HopClosestNeighbors
  with PositionInRouteAndRouteNr
  with NodesOfVehicle
  with PenaltyForUnroutedAsObjectiveTerm
  with HotSpot {
  installCostMatrix(distanceMatrix)
  setUnroutedPenaltyWeight(unroutedPenalty)
  computeClosestNeighbors()
}

object RoutingTestHotSpot extends App with StopWatch{

  this.startWatch()

  val n = 300
  val v = 10

  println("RoutingTest(n:" + n + " v:" + v + ")")

  val (distanceMatrix,positions) = RoutingMatrixGenerator(n,100)

  println("compozed matrix " + getWatch + "ms")

  val model = new Store()

  val vrp = new MyVRP(n,v,model,distanceMatrix,1000)

  model.close()

  println("closed model " + getWatch + "ms")

  val insertPointRoutedFirst = new InsertPointRoutedFirst(
    insertionPoints = vrp.routed,
    unroutedNodesToInsert = () => vrp.kNearest(10,!vrp.isRouted(_)),
    vrp = vrp)

  val insertPointUnroutedFirst = Profile(InsertPointUnroutedFirst(
    unroutedNodesToInsert= vrp.unrouted,
    relevantNeighbors = () => vrp.kNearest(20,vrp.isRouted(_)),
    vrp = vrp))

  //the other insertion point strategy is less efficient, need to investigate why.
  val insertPoint = insertPointUnroutedFirst

  val onePointMove = Profile(OnePointMove(
    nodesPrecedingNodesToMove = vrp.routed,
    relevantNeighbors= () => vrp.kNearest(20),
    vrp = vrp))

  //the one point move with HotSpot. notice that it is smalle than without hotspot because the pirmary must be hot
  //however, there could be move with primary cold and secondary hot; these are not explored (in the todo-list)
  val onePointHotView = vrp.newHotSpotView(true)
  val onePointHot = Profile(OnePointMove(
    nodesPrecedingNodesToMove = () => onePointHotView.hotNodesByVehicleWithConsumption,
    relevantNeighbors= () => vrp.kNearest(20),
    vrp = vrp,
    neighborhoodName = "OnePointMoveHot",
    hotRestart = false))

  val twoOpt = Profile(TwoOpt(
    predecesorOfFirstMovedPoint = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp))

  val twoOptMoveHotView = vrp.newHotSpotView(true)
  val twoOptHot = Profile(TwoOpt(
    predecesorOfFirstMovedPoint = () => twoOptMoveHotView.hotNodesByVehicleWithConsumption,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp,
    neighborhoodName = "TwoOptHot",
    hotRestart = false))


  val threeOpt = Profile(ThreeOpt(
    potentialInsertionPoints = vrp.routed,
    relevantNeighbors = () => vrp.kNearest(20),
    vrp = vrp))

  val searchHot = () => {vrp.resetHotSpotState();("Hot",(insertPoint exhaustBack (new BestSlopeFirst(List(onePointHot,twoOptHot,threeOpt)))) afterMove(vrp.updateHotSpotAnyMove(_)))}

  val search = () => ("standard",(insertPoint exhaustBack (new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))))

  //  search.verbose = 1
  //  search.verboseWithExtraInfo(3,() => vrp.toString)

  //  onePointMove.verboseWithExtraInfo(3,() => vrp.toString)

  //  search.doAllMoves(_ > 10*n, vrp.objectiveFunction)

  println(Benchmark.benchToStringFull(vrp.objectiveFunction,10,List(searchHot),0))

  //println("total time " + getWatch + "ms or  " + getWatchString)

  //println(search.profilingStatistics)

  //  println("\nresult:\n" + vrp)
}
