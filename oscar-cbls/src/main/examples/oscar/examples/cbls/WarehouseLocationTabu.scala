package oscar.examples.cbls

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Store}
import oscar.cbls.invariants.lib.logic.{Filter, SelectLESetQueue}
import oscar.cbls.invariants.lib.minmax.MinArray
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.AssignNeighborhood
import oscar.cbls.search.move.Move

/**
 * this is a WarehouseLocation problem with a Tabu.
 * the purpose is to illustrate how standard neighborhoods can be tuned to encompass
 * additional behaviors. Here, we restrict a neighborhood to a specific set of variables that not tabu
 * this set of variables is maintained through invariants
 */
object WarehouseLocationTabu extends App with AlgebraTrait{

  //the number of warehouses
  val W:Int = 15

  //the number of delivery points
  val D:Int = 150

  val tabuTenure = W/3

  println("WarehouseLocationTabu(W:" + W + ", D:" + D + ")")

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

  val warehouseOpenArray = Array.tabulate(W)(w => CBLSIntVar(m, 0 to 1, 0, "warehouse_" + w + "_open"))

  //We store in each warehouse variable its warehouse ID, using the
  // [[oscar.cbls.invariants.core.computation.DistributedStorageUtility]] mechanism
  //so we first ask a storageKey to the model
  val warehouseKey = m.newStorageKey()
  m.storeIndexesAt(warehouseOpenArray, warehouseKey)

  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinArray(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse).toIntVar("distance_for_delivery_" + d))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses))

  // we handle the tabu through invariants.
  // notice that they are completely dissociated from the rest of the model in this case.
  val TabuArray = Array.tabulate(W)(w => CBLSIntVar(m))
  val It = CBLSIntVar(m)
  val nonTabuWarehouses = SelectLESetQueue(TabuArray,It).toSetVar("non tabu warehouses")

  m.close()

  //this composite neighborhood includes:
  // *the search part restricted to non tabu warehouses
  // *the update of the tabu and iteration count
  // *the stop criterion based on maxMoves since last improvement over obj
  // *the protection of the objectiveFunction
  val switchWithTabuNeighborhood = (AssignNeighborhood(warehouseOpenArray, "SwitchWarehouseTabu",
    searchZone = nonTabuWarehouses, best = true)
    beforeMove((mo:Move) => {
    for (v <- mo.touchedVariables) {
      TabuArray(v.getStorageAt[Int](warehouseKey)) := It.value + tabuTenure
    }
    It :+= 1 }) acceptAll() maxMoves W withoutImprovementOver obj protectBest obj restoreBestOnExhaust)

  switchWithTabuNeighborhood.verbose = 1

  //all moves are accepted because the neighborhood returns the best found move, and tabu might degrade obj.
  switchWithTabuNeighborhood.doAllMoves(obj=obj)

  println(openWarehouses)
}
