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

package oscar.examples.cbls.wlpGraph

import oscar.cbls._
import oscar.cbls.algo.graph.{ConditionalGraphWithIntegerNodeCoordinates, FloydWarshall, Node, NodeWithIntegerCoordinates}
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.search.KSmallest
import oscar.cbls.lib.invariant.graph._
import oscar.cbls.lib.invariant.logic.Filter
import oscar.cbls.lib.invariant.numeric.Sum
import oscar.cbls.lib.invariant.set.Cardinality
import oscar.cbls.lib.search.combinators.{BestSlopeFirst, Mu, Profile}
import oscar.cbls.lib.search.neighborhoods._
import oscar.cbls.util.StopWatch
import oscar.cbls.visual.graph.GraphViewer
import oscar.cbls.visual.{ColorGenerator, SingleFrameWindow}

import scala.collection.immutable.SortedMap
import scala.language.postfixOps
import scala.swing.Color

object WarehouseAndBridgeLocation extends App with StopWatch{

  //the number of warehouses
  val W:Int = 1000

  //the number of delivery points
  val D:Int = 1000

  //nb conditional edges
  val nbConditionalEdges:Int = 200

  //nb non conditional edges
  val nbNonConditionalEdges =  (W+D)*5

  val displayDelay = 100

  println("WarehouseAndBridgeLocation(W:" + W + " D:" + D + " B:" + nbConditionalEdges + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  println("generate random graph")
  val graph = RandomGraphGenerator.generatePseudoPlanarConditionalGraph(
    nbNodes=(W+D),
    nbConditionalEdges=nbConditionalEdges,
    nbNonConditionalEdges=nbNonConditionalEdges,
    mapSide = 1000)


  //println("floyd")
  //val distanceMatrix = FloydWarshall.buildDistanceMatrix(graph, _ => true):Array[Array[Option[Int]]]

  //warehouses are numbered by nodeID from 0 to W-1
  //shops are numbered by ndeID from W to W+D-1
  val warehouseToNode =  Array.tabulate(W)(w => graph.nodeswithCoordinates(w))
  val deliveryToNode = Array.tabulate(D)(d => graph.nodeswithCoordinates(d + W))

  val deliveryNodeList = QList.buildFromIterable(deliveryToNode).asInstanceOf[QList[Node]]

  println("start floyd")
  val underApproximatingDistanceInGraphAllCondtionsOpen:Array[Array[Long]] = FloydWarshall.buildDistanceMatrix(graph, _ => true)
  println("end floyd")

  //val warehouseToWarehouseDistances = Array.tabulate(W)(w1 => Array.tabulate(W)(w2 => distanceMatrix(w1)(w2).getOrElse(1000)))
  val warehouseToWarehouseDistances:Array[Array[Long]] =
    Array.tabulate(W)(w1 => Array.tabulate(W)(w2 =>  underApproximatingDistanceInGraphAllCondtionsOpen(warehouseToNode(w1).nodeId)(warehouseToNode(w2).nodeId)))

  val costForOpeningWarehouse =  Array.fill[Long](W)(800)

  val m = Store() //checker = Some(new ErrorChecker()))
  println("model")

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")


  val edgeConditionArray = Array.tabulate(nbConditionalEdges)(c => CBLSIntVar(m, 1, 0 to 1, "edgeCondition_" + c + "_open"))

  val openConditions = Filter(edgeConditionArray).setName("openConditions")

  println("Creating Voronoï zones invariant")

  val vor = VoronoiZones(graph,
    graphDiameterOverApprox = Int.MaxValue,
    openConditions = openConditions,
    centroids = openWarehouses,
    trackedNodes = deliveryToNode.map(_.nodeId:Long),
    m,
    defaultDistanceForUnreachableNodes = 10000)

  val trackedNodeToDistanceAndCentroid: SortedMap[Long,(CBLSIntVar,CBLSIntVar)] = vor.trackedNodeToDistanceAndCentroidMap

  println("done init voronoï zones")

  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    trackedNodeToDistanceAndCentroid(deliveryToNode(d).nodeId)._1)

  val costOfBridgesPerBridge = 7
  

  val selectedDistances = Array.tabulate(20)(w =>
    new DistanceInConditionalGraph(graph,0,w,openConditions,10000)(underApproximatingDistanceInGraphAllCondtionsOpen(_)(_)))
  val totalDistanceToWs = sum(selectedDistances)
  val x = Cardinality(openConditions)

  val obj = Objective(Sum(distanceToNearestOpenWarehouseLazy) + Sum(costForOpeningWarehouse, openWarehouses) + (x*costOfBridgesPerBridge) + totalDistanceToWs)

  m.close()

  val centroidColors = ColorGenerator.generateRandomColors(W)

  val visual = new GraphViewer(graph:ConditionalGraphWithIntegerNodeCoordinates,
    centroidColor = SortedMap.empty[Int,Color] ++ warehouseToNode.toList.map(node => (node.nodeId,centroidColors(node.nodeId))))

  SingleFrameWindow.show(visual,title = "Warehouse and bridge location")

  var bestDisplayedObj = Int.MaxValue

  //this is an array, that, for each warehouse, keeps the sorted closest warehouses in a lazy way.
  val closestWarehouses = Array.tabulate(W)(warehouse =>
    KSmallest.lazySort(
      Array.tabulate(W)(warehouse => warehouse),
      otherWarehouse => warehouseToWarehouseDistances(warehouse)(otherWarehouse)
    ))

  //this procedure returns the k closest closed warehouses
  def kNearestClosedWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue == 0)
  //this procedure returns the k closest open warehouses
  def kNearestOpenWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse), filter = (otherWarehouse) => warehouseOpenArray(otherWarehouse).newValue != 0)
  def kNearestdWarehouses(warehouse:Int,k:Int) = KSmallest.kFirst(k, closestWarehouses(warehouse))

  def muLine(depth:Int,kOpen:Int,kClosed:Int ) = Mu[AssignMove](
    AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
    (assignList:List[AssignMove]) =>
    {
      val lastChangedWarehouse = assignList.head.id
      val setTo = assignList.head.v
      val otherWarehouses = if(setTo == 0) kNearestClosedWarehouses(lastChangedWarehouse,kClosed) else kNearestOpenWarehouses(lastChangedWarehouse,kOpen)
      Some(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse",searchZone = () => otherWarehouses,hotRestart = false))
    },
    maxDepth = depth,
    intermediaryStops = true)


  def muStar(width:Int,kOpen:Int,kClosed:Int ) = Mu[AssignMove](
    AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"),
    (assignList:List[AssignMove]) =>
    {
      val lastChangedWarehouse = assignList.last.id
      val setTo = assignList.head.v
      val otherWarehouses = if(setTo == 0) kNearestClosedWarehouses(lastChangedWarehouse,kClosed) else kNearestOpenWarehouses(lastChangedWarehouse,kOpen)
      Some(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse",searchZone = () => otherWarehouses,hotRestart = false))
    },
    maxDepth = width,
    intermediaryStops = true)

  def swapsK(k:Int,openWarehoueseTocConsider:()=>Iterable[Long] = openWarehouses) = SwapsNeighborhood(warehouseOpenArray,
    searchZone1 = openWarehoueseTocConsider,
    searchZone2 = () => (firstWareHouse,_) => kNearestClosedWarehouses(firstWareHouse,k),
    name = "Swap" + k + "Nearest",
    symmetryCanBeBrokenOnIndices = false)

  def doubleSwap(k:Int) = (swapsK(k) dynAndThen((firstSwap:SwapMove) => swapsK(k,() => kNearestOpenWarehouses(firstSwap.idI,k)))) name "DoubleSwap"

  var lastDisplay = this.getWatch

  println("\t" + selectedDistances.mkString("\n\t"))

  def getFactorApartBridges(w1:Int,w2:Int,factor:Int):Iterable[Long] = {
    val nodeW1 = warehouseToNode(w1)
    val nodeW2 = warehouseToNode(w2)

    val distanceW1W2:Long = underApproximatingDistanceInGraphAllCondtionsOpen(nodeW1.nodeId)(nodeW2.nodeId)

    (0L until nbConditionalEdges).filter(c => {
      val conditionalEdge = graph.conditionToConditionalEdges(c)
      val distA1 = underApproximatingDistanceInGraphAllCondtionsOpen(conditionalEdge.nodeA.nodeId)(nodeW1.nodeId)
      val distA2 = underApproximatingDistanceInGraphAllCondtionsOpen(conditionalEdge.nodeA.nodeId)(nodeW2.nodeId)
      val distB1 = underApproximatingDistanceInGraphAllCondtionsOpen(conditionalEdge.nodeB.nodeId)(nodeW1.nodeId)
      val distB2 = underApproximatingDistanceInGraphAllCondtionsOpen(conditionalEdge.nodeB.nodeId)(nodeW2.nodeId)

      ((distA1 + distB2) min (distA2 + distB1)) < distanceW1W2*factor
    })
  }

  val factorForFastCombined = 2


  println("start warehousesPairToTwiceApartBridges")
  //we only fill half of the matrix; it is symmetric anyway, so just use the upper part
  //Also we use a for below to enable parallelism, since this is brutal computation
  val warehousesPairToTwiceApartBridges:Array[Array[Iterable[Long]]] = Array.fill(W)(null)

  for(w1 <- (0 until W).par){
    warehousesPairToTwiceApartBridges(w1) = Array.tabulate(W)(w2 => (if(w1 > w2) null else (getFactorApartBridges(w1,w2,factorForFastCombined))))
  }
  println("end warehousesPairToTwiceApartBridges")

  val neighborhood =(
    BestSlopeFirst(
      List(
        Profile(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")),
        Profile(AssignNeighborhood(edgeConditionArray, "SwitchConditions")),
        Profile(SwapsNeighborhood(edgeConditionArray, "SwapConditions")),
        Profile(swapsK(20) guard(() => openWarehouses.value.size >= 5)), //we set a minimal size because the KNearest is very expensive if the size is small
        Profile(swapsK(20)
          dynAndThen((s:SwapMove) => AssignNeighborhood(edgeConditionArray, searchZone = ()=>warehousesPairToTwiceApartBridges(s.idI min s.idJ)(s.idI max s.idJ), name ="FastSwitchConditionsCombined"))
          guard(() => openWarehouses.value.size >= 5)
          name "fastCombined"),
        //Profile(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses") guard(() => openWarehouses.value.size >= 5))
      ),refresh = W/10)

      //TODO: proposer aussi maxResstart!
      //TODO: vérifier quon restart bien du best so far.
      onExhaustRestartAfter(RandomizeNeighborhood(warehouseOpenArray, () => W/5), 10, obj, restartFromBest = false)
    exhaust        Profile((swapsK(20) andThen AssignNeighborhood(edgeConditionArray, "SwitchConditionsCombined")) guard(() => openWarehouses.value.size >= 5) name "combined"), //we set a minimal size because the KNearest is very expensive if the size is small

    ) afterMove(
    if(lastDisplay + displayDelay <= this.getWatch){ //} && obj.value < bestDisplayedObj) {
      bestDisplayedObj = obj.value

      visual.redraw(
        openConditions.value,
        openWarehouses.value,
        trackedNodeToDistanceAndCentroid.mapValues({ case (v1, v2) => v2.value}),
        hideClosedEdges = false,
        hideRegularEdges = false,
        hideOpenEdges = false,
        emphasizeEdges = vor.spanningTree(deliveryNodeList),
        selectedDistances.map(_.getPath)
      )

      lastDisplay = this.getWatch
    })

  neighborhood.verbose = 1

  neighborhood.doAllMoves(obj=obj)

  visual.redraw(
    openConditions.value,
    openWarehouses.value,
    trackedNodeToDistanceAndCentroid.mapValues({case (v1,v2) => v2.value}),
    hideClosedEdges = true,
    emphasizeEdges = vor.spanningTree(deliveryNodeList),
    hideRegularEdges = true,
    hideOpenEdges=false,
    extraPath = selectedDistances.map(_.getPath))

  println(neighborhood.profilingStatistics)

  println(openWarehouses)
  println(openConditions)
  println("\t" + selectedDistances.mkString("\n\t"))
}
