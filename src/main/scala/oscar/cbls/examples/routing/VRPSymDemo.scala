package oscar.cbls.examples.routing

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

import oscar.cbls._
import oscar.cbls.business.routing._
import oscar.cbls.business.routing.utils.RoutingMatrixGenerator
import oscar.cbls.core.search.{Best, First}
import oscar.cbls.util.StopWatch



object VRPDemo extends App {

  println("usage: VRPDemo n v")
  val n:Int=args(0).toInt
  val v = args(1).toInt

  val displayDelay = if (n >= 1000) 1000 else 100 //ms
  val verbose = 1
  val maxPivotPerValuePercent = 4
  val mapSide = 1000

  new VRPDemo(n,v,maxPivotPerValuePercent,verbose,displayDelay, mapSide)
}

class VRPDemo(n:Int, v:Int, maxPivotPerValuePercent:Int, verbose:Int, displayDelay:Int, mapSide:Int) extends StopWatch{

  val (symmetricDistanceMatrix,nodesPositions) = RoutingMatrixGenerator(n,side=mapSide)

  startWatch()
  val model = new Store()

  val myVRP = new VRP(model,n,v)
  val routingDistance = constantRoutingDistance(myVRP.routes,n,v,false,symmetricDistanceMatrix,true,true,false)(0)
  val graphicExtension = display(myVRP,nodesPositions.map(np => (np._1.toDouble,np._2.toDouble)).toList,sizeOfMap = Some(mapSide), refreshRate = displayDelay, title = "symmetric TSP (n=" + n + " v=" + v + ")")
  val penaltyForUnrouted  = 10000

  val obj = Objective(routingDistance + (penaltyForUnrouted*(n - length(myVRP.routes))))

  model.close()

  val allNodes = (node:Int) => myVRP.nodes

  val closestRelevantPredecessors = Array.tabulate(n)(DistanceHelper.lazyClosestPredecessorsOfNode(symmetricDistanceMatrix,allNodes))

  val closestRelevantSuccessors = Array.tabulate(n)(DistanceHelper.lazyClosestSuccessorsOfNode(symmetricDistanceMatrix,allNodes))

  val routedPostFilter = (node:Int) => (neighbor:Int) => myVRP.isRouted(neighbor)
  val unRoutedPostFilter = (node:Int) => (neighbor:Int) => !myVRP.isRouted(neighbor)

  val routeUnroutedPoint =  profile(insertPointUnroutedFirst(myVRP.unrouted,
    ()=>myVRP.kFirst(10,closestRelevantPredecessors,routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false,
    selectNodeBehavior = First(),
    selectInsertionPointBehavior = Best(maxNeighbors = () => 10)))

  val routeUnroutedPointBad =  profile(insertPointUnroutedFirst(myVRP.unrouted,
    ()=> myVRP.kFirst(20,closestRelevantPredecessors,routedPostFilter),
    myVRP,
    neighborhoodName = "InsertUF",
    hotRestart = false))


  //using post-filters on k-nearest is probably a bit slower than possible for large problems.
  //that's why we prefer to block this neighborhood when many nodes are already routed (so few are unrouted, so the filter filters many nodes away)
  val routeUnroutedPoint2 =  profile(insertPointRoutedFirst(
    myVRP.routed,
    ()=>myVRP.kFirst(10,closestRelevantSuccessors,unRoutedPostFilter),
    myVRP,
    selectInsertionPointBehavior = Best(maxNeighbors = ()=>10),
    selectInsertedNodeBehavior = First(),
    neighborhoodName = "InsertRF")
    guard(() => myVRP.routed.value.size < n/5))

  val routeUnroutedPoint3 =  profile(insertPointRoutedFirst(
    myVRP.routed,
    ()=>myVRP.kFirst(10,closestRelevantSuccessors,unRoutedPostFilter),
    myVRP,
    selectInsertionPointBehavior = First(),
    selectInsertedNodeBehavior = First(),
    neighborhoodName = "InsertRF")
    guard(() => myVRP.routed.value.size < n/2 && myVRP.routed.value.size >= n/5))

  def onePtMove(k:Int) = profile(onePointMove(
    myVRP.routed,
    () => myVRP.kFirst(k,closestRelevantPredecessors,routedPostFilter),
    myVRP,
    selectDestinationBehavior = Best()))

  val customTwoOpt = profile(twoOpt(myVRP.routed, ()=>myVRP.kFirst(20,closestRelevantPredecessors,routedPostFilter), myVRP))

  def customThreeOpt(k:Int, breakSym:Boolean) =
    profile(threeOpt(myVRP.routed, ()=>myVRP.kFirst(k,closestRelevantPredecessors,routedPostFilter), myVRP,breakSymmetry = breakSym, neighborhoodName = "ThreeOpt(k=" + k + ")"))

  val vlsn1pt = mu[OnePointMoveMove](
    onePointMove(myVRP.routed, () => myVRP.kFirst(5,closestRelevantPredecessors,routedPostFilter),myVRP),
    l => Some(onePointMove(() => List(l.head.newPredecessor).filter(_ >= v), () => myVRP.kFirst(3,closestRelevantPredecessors,routedPostFilter),myVRP, hotRestart = false)),
    intermediaryStops = true,
    maxDepth = 6)

  def segExchange(k:Int) = segmentExchange(myVRP,()=>myVRP.kFirst(k,closestRelevantPredecessors,routedPostFilter), () => myVRP.vehicles) guard(() => {v > 1})


  graphicExtension.drawRoutes()
  Thread.sleep(10000)
  println("start")
  Thread.sleep(1000)
  //le problème du hérisson apparaît lorsuqe routeUnroutedPoint3 & routeUnroutedPoint3 sont actifs
  val search = (bestSlopeFirst(List(routeUnroutedPoint,  routeUnroutedPoint2,  /*routeUnroutedPoint3, */ vlsn1pt, onePtMove(10),customTwoOpt, customThreeOpt(10,true),segExchange(10)))
    exhaust bestSlopeFirst(List(customThreeOpt(30,true),vlsn1pt))).afterMove(graphicExtension.drawRoutes()) showObjectiveFunction(obj)

  search.verbose = verbose
  //search.verboseWithExtraInfo(1, ()=> "" + myVRP)
//  routeUnroutdPoint.verbose= 4
  search.doAllMoves(obj = obj)

  graphicExtension.drawRoutes()
  print(myVRP)
}
