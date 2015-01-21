package oscar.examples.cbls

import oscar.cbls.invariants.core.computation.{CBLSIntConst, CBLSIntVar, IntValue, Store}
import oscar.cbls.invariants.lib.logic.Filter
import oscar.cbls.invariants.lib.minmax.{MinConstArray, Min, MinArray}
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.{AssignNeighborhood, RandomizeNeighborhood, SwapsNeighborhood}

import scala.language.postfixOps

object WarehouseLocation extends App with AlgebraTrait{

  //the number of warehouses
  val W:Int = 15

  //the number of delivery points
  val D:Int = 150

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  // we put the locations randomly on a square map
  val minXY = 0
  val maxXY = 100
  val side = maxXY - minXY

  val weightingForOpeningWarehouseCost = 3

  val costForOpeningWarehouse:Array[Int] =
    Array.tabulate(W)(w => (math.random * side * weightingForOpeningWarehouseCost).toInt)

  //we generate te cost distance matrix
  def randomXY:Int = (minXY + (math.random * side)).toInt
  def randomPosition = (randomXY,randomXY)
  val warehousePositions:Array[(Int,Int)] = Array.tabulate(W)(w => randomPosition)
  val deliveryPositions:Array[(Int,Int)] = Array.tabulate(D)(d => randomPosition)
  def distance(from:(Int,Int), to:(Int, Int)) =
    math.sqrt(math.pow(from._1 - to._1,2) + math.pow(from._2 - to._2,2)).toInt

  //for each delivery point, the distance to each warehouse
  val distanceCost = Array.tabulate(D)(
    d => Array.tabulate(W)(
      w => distance(warehousePositions(w), deliveryPositions(d))))

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinConstArray(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse).setName("distance_for_delivery_" + d))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses))

  m.close()

  val neighborhood = (AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")
                      exhaustBack SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses")
                      orElse (RandomizeNeighborhood(warehouseOpenArray, W/5) maxMoves 2) protectBest obj restoreBestOnExhaust)

  //you can also use the following composite to replace SwapNeighborhood (but it will be slower than the Swap)
  //AssignNeighborhood(warehouseOpenArray, obj, "SwitchFirstWarehouse") maxMoves 5
  // andThen AssignNeighborhood(warehouseOpenArray, obj, "SwitchSecondWarehouse")

  neighborhood.verbose = 1
  neighborhood.doAllMoves(_ >= W+D, obj)

  println(openWarehouses)
}
