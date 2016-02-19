package oscar.examples.cbls.routing

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.lib.minmax.{Max, Min}
import oscar.cbls.invariants.lib.numeric.{Abs, Sum}
import oscar.cbls.objective.Objective
import oscar.cbls.routing.model._
import oscar.cbls.routing.neighborhood._
import oscar.cbls.search.StopWatch
import oscar.cbls.search.combinators.{Atomic, RoundRobin, Profile, BestSlopeFirst}
import oscar.cbls.modeling.Algebra._
import oscar.examples.cbls.routing.visual.FramedObjFunctionVisual
import scala.language.implicitConversions

/**
 * Created by rdl on 15-12-15.
 */


class MyVRP(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]],unroutedPenalty:Int)
  extends VRP(n,v,model)
  with HopDistanceAsObjectiveTerm
  with HopClosestNeighbors
  with PositionInRouteAndRouteNr
  with NodesOfVehicle
  with PenaltyForUnrouted
  with hopDistancePerVehicle
with PenaltyForEmptyRouteAsObjectiveTerm{ //just for the fun of it

  installCostMatrix(distanceMatrix)
  setUnroutedPenaltyWeight(unroutedPenalty)
  closeUnroutedPenaltyWeight()
  computeClosestNeighbors()
  println("end compute closest, install matrix")
  installHopDistancePerVehicle()
  println("end install matrix, posting constraints")

  //evenly spreading the travel among vehicles
  val averageDistanceOnAllVehicles = overallDistance / V
  val spread = Sum(hopDistancePerVehicle.map(h => Abs(h - averageDistanceOnAllVehicles)))
  addObjectiveTerm(spread)
  addObjectiveTerm(unroutedPenalty)

  setEmptyRoutePenaltyWeight(100)
  println("vrp done")
}

object RoutingTest extends App with StopWatch{

  this.startWatch()

  val objGraphic = new FramedObjFunctionVisual()

  val n = 300
  val v = 5

  println("RoutingTest(n:" + n + " v:" + v + ")")

  val (distanceMatrix,positions) = RoutingMatrixGenerator(n,10000)

  println("compozed matrix " + getWatch + "ms")

  val model = new Store()

  val vrp = new MyVRP(n,v,model,distanceMatrix,100000)

  model.close()

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

  val compositeInsertPoint = Profile((insertPointRoutedFirst guard(() => vrp.unrouted.value.size >= pivot)
    orElse (insertPointUnroutedFirst guard(() => vrp.unrouted.value.size < pivot))))

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

  val search = new RoundRobin(List(insertPoint,onePointMove),10) exhaust
                      (new BestSlopeFirst(List(onePointMove,threeOpt,segExchange),refresh = n/2)) afterMove({
    objGraphic.saveObjValue(vrp.getObjective().value,getWatch)
  }) // exhaust onePointMove exhaust segExchange//threeOpt //(new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))

  search.verbose = 1
//    search.verboseWithExtraInfo(3,() => vrp.toString)
  //segExchange.verbose = 3
  search.doAllMoves(_ > 10*n, vrp.objectiveFunction)
  objGraphic.drawGlobalCurve()

  println("total time " + getWatch + "ms or  " + getWatchString)

  println("\nresult:\n" + vrp)

  println(search.profilingStatistics)
}
