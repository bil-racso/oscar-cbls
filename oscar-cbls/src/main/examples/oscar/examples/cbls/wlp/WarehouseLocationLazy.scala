package oscar.examples.cbls.wlp

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Store}
import oscar.cbls.invariants.lib.logic.Filter
import oscar.cbls.invariants.lib.minmax.{MinConstArrayLazy, MinConstArray}
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.combinators.{LearningRandom, BiasedRandom, Statistics}
import oscar.cbls.search.{Benchmark, AssignNeighborhood, RandomizeNeighborhood, SwapsNeighborhood}

import scala.language.postfixOps

object WarehouseLocationLazy extends App with AlgebraTrait{

  //the number of warehouses
  val W:Int = 500

  //the number of delivery points
  val D:Int = 150

  println("WarehouseLocation(W:" + W + ", D:" + D + ")")
  //the cost per delivery point if no location is open
  val defaultCostForNoOpenWarehouse = 10000

  val (costForOpeningWarehouse,distanceCost) = WarehouseLocationGenerator.apply(W,D,0,100,3)

  val m = Store()

  val warehouseOpenArray = Array.tabulate(W)(l => CBLSIntVar(m, 0, 0 to 1, "warehouse_" + l + "_open"))
  val openWarehouses = Filter(warehouseOpenArray).setName("openWarehouses")
  m.registerForPartialPropagation(openWarehouses)
  val openWarehouses2 = Filter(warehouseOpenArray).setName("openWarehouses")
  m.registerForPartialPropagation(openWarehouses2)

  val distanceToNearestOpenWarehouse = Array.tabulate(D)(d =>
    MinConstArray(distanceCost(d), openWarehouses2, defaultCostForNoOpenWarehouse).setName("distance_for_delivery_" + d))

  val obj = Objective(Sum(distanceToNearestOpenWarehouse) + Sum(costForOpeningWarehouse, openWarehouses2))

  val distanceToNearestOpenWarehouseLazy = Array.tabulate(D)(d =>
    MinConstArrayLazy(distanceCost(d), openWarehouses, defaultCostForNoOpenWarehouse,100))//.setName("distance_for_delivery_" + d))

  val objLazy = Objective(Sum(distanceToNearestOpenWarehouseLazy) + Sum(costForOpeningWarehouse, openWarehouses))


  m.close()

  val neighborhood = (Statistics(
    Statistics(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse"))
      step Statistics(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses"))))

  neighborhood.verbose = 0

  val startSolution = m.solution()
  m.propagate()

  neighborhood.doAllMoves(obj=obj)
  println(openWarehouses2)

  println(neighborhood.statistics)
  val nonLazyStats = neighborhood.statistics

  m.restoreSolution(startSolution)
  m.propagate()
  neighborhood.resetStatistics()
  neighborhood.reset()

  neighborhood.doAllMoves(obj=objLazy)
  println(openWarehouses)

  println("lazyStats:")
  println(neighborhood.statistics)

  println("non lazyStats:")
  println(nonLazyStats)

  val nbAnihilation = distanceToNearestOpenWarehouseLazy.toList.map((a:MinConstArrayLazy) => a.nbAnihilation).sum
  val nbDoIt = distanceToNearestOpenWarehouseLazy.toList.map((a:MinConstArrayLazy) => a.nbDoIt).sum
  println("nbAnihilation : " + nbAnihilation)
  println("nbDoIt: " + nbDoIt)
  println("ratio:" + nbAnihilation.toDouble/(nbDoIt.toDouble + nbAnihilation.toDouble))
  //  println("model stats:")
  //  println(m.stats)

}
