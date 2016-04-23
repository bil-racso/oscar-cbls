package oscar.examples.cbls.routing

import java.awt.Toolkit
import javax.swing.JFrame

import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.invariants.lib.logic.Int2Int
import oscar.cbls.invariants.lib.numeric.{Abs, Sum}
import oscar.cbls.routing.model._
import oscar.cbls.routing.neighborhood._
import oscar.cbls.search.StopWatch
import oscar.cbls.search.combinators.{RoundRobin, Profile, BestSlopeFirst}
import oscar.cbls.modeling.Algebra._
import oscar.examples.cbls.routing.visual.ColorGenerator
import oscar.examples.cbls.routing.visual.MatrixMap.{RoutingMatrixVisualWithAttribute, PickupAndDeliveryMatrixVisualWithAttribute}
import oscar.visual.VisualFrame
import scala.language.implicitConversions

/**
 * Created by rdl on 15-12-15.
 */


class MyVRP(n:Int, v:Int, model:Store, distanceMatrix: Array[Array[Int]],unroutedPenaltyWeight:Int, loadBalancing:Double = 1.0)
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

object RoutingTest extends App with StopWatch{

  this.startWatch()

  val n = 50
  val v = 1

  println("RoutingTest(n:" + n + " v:" + v + ")")

  val (distanceMatrix,positions) = RoutingMatrixGenerator(n,10000)

  println("compozed matrix " + getWatch + "ms")

  val model = new Store()

  val vrp = new MyVRP(n,v,model,distanceMatrix,100000)

  model.close()

  println("closed model " + getWatch + "ms")

  val f = new JFrame("ROUTING - Routing Map")
  f.setSize(Toolkit.getDefaultToolkit.getScreenSize.getWidth.toInt,(11*Toolkit.getDefaultToolkit().getScreenSize().getHeight/12).toInt)
  val rm = new RoutingMatrixVisualWithAttribute("Routing Map",vrp,10000,positions.toList,ColorGenerator.generateRandomColors(v),dimension = f.getSize)
  f.add(rm)
  f.pack()
  f.setVisible(true)

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

  //val linKernighanMulti = Profile(BestSlopeFirst(List(LinKernighan(0,vrp), LinKernighan(1,vrp), LinKernighan(2,vrp), LinKernighan(3,vrp), LinKernighan(4,vrp))))
  val linKernighanOne = Profile(LinKernighan(0,vrp))
  /*val kernighanSearchMulti = insertPoint exhaust BestSlopeFirst(List(linKernighan, segExchange)) showObjectiveFunction vrp.objectiveFunction afterMove {
    rm.drawRoutes()
  }
  kernighanSearchMulti.verbose = 1*/

  val kernighanSearchOne = insertPoint exhaust linKernighanOne showObjectiveFunction vrp.objectiveFunction afterMove rm.drawRoutes()
  kernighanSearchOne.verbose = 1
  /*val search = new RoundRobin(List(insertPoint,onePointMove),10) exhaust
                      new BestSlopeFirst(List(onePointMove, threeOpt, segExchange), refresh = n / 2) showObjectiveFunction vrp.getObjective()
  // exhaust onePointMove exhaust segExchange//threeOpt //(new BestSlopeFirst(List(onePointMove,twoOpt,threeOpt)))

  search.verbose = 1*/
//    search.verboseWithExtraInfo(3,() => vrp.toString)
  //segExchange.verbose = 3

  def launchSearch(): Unit ={
    kernighanSearchOne.doAllMoves(_ > 10*n, vrp.objectiveFunction)

    println("total time " + getWatch + "ms or  " + getWatchString)

    println("\nresult:\n" + vrp)

    println(kernighanSearchOne.profilingStatistics)
  }

  launchSearch()
}
