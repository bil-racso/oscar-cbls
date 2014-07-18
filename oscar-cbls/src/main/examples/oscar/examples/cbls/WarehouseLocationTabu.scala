package oscar.examples.cbls

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Store}
import oscar.cbls.invariants.lib.logic.{SelectLESetQueue, Filter}
import oscar.cbls.invariants.lib.minmax.MinArray
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.set.{Interval, MakeSet}
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.move.Move
import oscar.cbls.search.{AssignNeighborhood, RandomizeNeighborhood, SwapsNeighborhood}

object WarehouseLocationTabu extends App with AlgebraTrait{

  //the number of warehouses
  val W:Int = 15

  //the number of delivery points
  val D:Int = 150

  
  val maxStepsWithNoImprovement = W
  val tabuTenure = W/3

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")

  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  // we put the locations randomly on a square map
  val minXY = 0
  val maxXY = 100
  val side = maxXY - minXY


  val weightingForOpeningWarehouseCost = 3

  val costForOpeningWarehouse:Array[Int] = Array.tabulate(W)(
    w => (math.random * side * weightingForOpeningWarehouseCost).toInt)

  //we generate te cost distance matrix
  def randomXY:Int = (minXY + (math.random * side)).toInt
  def randomPosition = (randomXY,randomXY)
  val warehousePositions:Array[(Int,Int)] = Array.tabulate(W)(w => randomPosition)
  val deliveryPositions:Array[(Int,Int)] = Array.tabulate(D)(d => randomPosition)
  def distance(from:(Int,Int), to:(Int, Int)) =
    math.sqrt(math.pow(from._1 - to._1,2) + math.pow(from._2 - to._2,2)).toInt

  //for each delivery point, the distance to each warehouse
  val distanceCost:Array[Array[Int]] = Array.tabulate(D)(
    d => Array.tabulate(W)(
      w => distance(warehousePositions(w), deliveryPositions(d))))

  val m = Store()

  //We store in each warehouse variable its warehouse ID,
  //so we first ask a storageKey to the model
  val warehouseKey = m.getStorageIndex()

  val warehouseOpenArray = Array.tabulate(W)(w => {
    val wVar = CBLSIntVar(m, 0 to 1, 0, "warehouse_" + w + "_open")
    wVar.storeAt(warehouseKey,new Integer(w))
    wVar
  })

  val openWarehouses = Filter(warehouseOpenArray).toSetVar("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinArray(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse).toIntVar("distance_for_delivery_" + d))

  val totalCost = Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses)

  val obj = Objective(totalCost)

  val TabuArray = Array.tabulate(W)(w => CBLSIntVar(m))
  val It = CBLSIntVar(m)

  val neighborhood = (AssignNeighborhood(warehouseOpenArray, obj, "SwitchWarehouseTabu",
    searchZone = SelectLESetQueue(TabuArray,It), best = true)
    onMove((mo:Move) => {
    for (v <- mo.touchedVariables) {
      val i = v.getStorageAt[Int](warehouseKey,-1)
      TabuArray(i) := It.value + tabuTenure
      It :+= 1
    }
  }) protectBest obj)

  m.close()

  neighborhood.verbose = 1

  //we accept all moves since the neighborhood is required to return the best found move.

  var oldObj = obj.value
  var stepsSinceLastImprovement = 0
  neighborhood.doAllImprovingMoves((it:Int) => {
    if(obj.value < oldObj) {oldObj = obj.value; stepsSinceLastImprovement = 0}
    else stepsSinceLastImprovement +=1
    it >= W+D || stepsSinceLastImprovement >= maxStepsWithNoImprovement},
    (oldObj,newObj) => true)

  neighborhood.restoreBest()

  println(openWarehouses)

}

