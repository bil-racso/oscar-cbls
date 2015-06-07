package oscar.cp.constraints

import oscar.cp._
import oscar.cp.scheduling.visual.{VisualProfile, VisualGanttChart}
import oscar.visual.{VisualUtil, VisualFrame}

/**
 * Created on 03/06/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */

/**
 * @constructor Create a new reservoir resource with specified parameters. For any point of time,
 *              the amount of resource in the reservoir must be between its minimal and maximal capacity
 *
 * @param startVars Variables for task starting times
 * @param durationVars Variables for task durations
 * @param endVars Variables for task ending times
 * @param productionVars Variables for task productions; represents the amounts of the resource produced by tasks
 * @param consumptionVars Variables for task consumptions; represents the amounts of the resource consumed by tasks
 * @param minCapacity The minimal capacity of the reservoir
 * @param maxCapacity The maximal capacity of the reservoir
 * @param initialAmount The initial amount of resource in the reservoir
 */
class ReservoirResource(startVars: Array[CPIntVar], durationVars: Array[CPIntVar], endVars: Array[CPIntVar], productionVars: Array[CPIntVar], consumptionVars: Array[CPIntVar], minCapacity: Int, maxCapacity: Int, initialAmount: Int = 0) {
  private[this] val nTasks = startVars.length
  val cpSolver: CPStore = startVars(0).store
  val startingPointOfTime = startVars.map(myVar => myVar.min).min
  val horizon = endVars.map(myVar => myVar.max).max
  val producer = Array.tabulate(nTasks)(i => productionVars(i).max > 0)

  /* Check if the maximal capacity of the reservoir is never exceeded.
   *
   * Represented with a cumulative resource as follows:
   * - Cumulative resource capacity <- \sum consumptionVars + maxCapacity - initialAmount
   * - Each consumer task is transformed into a new task spanning from starting point of time to the start var of former consumer
   * - Each producer task is transformed into a new task spanning from the end var of former producer to horizon
   */
  val sumConsumption = consumptionVars.map(cVar => cVar.max).sum
  val cumulativeCapacity1 = CPIntVar(sumConsumption + maxCapacity - initialAmount)(cpSolver)
  val cumulativeStart1 = Array.tabulate(nTasks)(i => if(!producer(i)) CPIntVar(startingPointOfTime)(cpSolver) else endVars(i))
  val cumulativeEnd1 = Array.tabulate(nTasks)(i => if(producer(i)) CPIntVar(horizon)(cpSolver) else startVars(i))
  val cumulativeDuration1 = Array.tabulate(nTasks)(i => CPIntVar((cumulativeEnd1(i).min - cumulativeStart1(i).max) to (cumulativeEnd1(i).max - cumulativeStart1(i).min))(cpSolver))
  val cumulativeDemand = Array.tabulate(nTasks)(i => if(producer(i)) productionVars(i) else consumptionVars(i))

  for (i <- 0 until nTasks) {
    cpSolver.add(cumulativeEnd1(i) - cumulativeStart1(i) == cumulativeDuration1(i))
  }
  cpSolver.add(maxCumulativeResource(cumulativeStart1, cumulativeDuration1, cumulativeEnd1, cumulativeDemand, cumulativeCapacity1))

  /* Check if reservoir amount is never under its minimal capacity.
   *
   * Represented with a cumulative resource as follows:
   * - Cumulative resource capacity <- \sum productionVars - minCapacity + initialAmount
   * - Each producer task is transformed into a new task spanning from starting point of time to the end var of former producer
   * - Each consumer task is transformed into a new task spanning from the start var of former consumer to horizon
   */
  val sumProduction = productionVars.map(cVar => cVar.max).sum
  val cumulativeCapacity2 = CPIntVar(sumProduction - minCapacity + initialAmount)(cpSolver)
  val cumulativeStart2 = Array.tabulate(nTasks)(i => if(!producer(i)) startVars(i) else CPIntVar(startingPointOfTime)(cpSolver))
  val cumulativeEnd2 = Array.tabulate(nTasks)(i => if(producer(i)) endVars(i) else CPIntVar(horizon)(cpSolver))
  val cumulativeDuration2 = Array.tabulate(nTasks)(i => CPIntVar((cumulativeEnd2(i).min - cumulativeStart2(i).max) to (cumulativeEnd2(i).max - cumulativeStart2(i).min))(cpSolver))

  for (i <- 0 until nTasks) {
    cpSolver.add(cumulativeEnd2(i) - cumulativeStart2(i) == cumulativeDuration2(i))
  }
  cpSolver.add(maxCumulativeResource(cumulativeStart2, cumulativeDuration2, cumulativeEnd2, cumulativeDemand, cumulativeCapacity2))
}

object ReservoirResource {
  /**
   * @constructor Create a new reservoir resource with specified parameters. For any point of time,
   *              the amount of resource in the reservoir must be between its minimal and maximal capacity
   *
   * @param startVars Variables for task starting times
   * @param durationVars Variables for task durations
   * @param endVars Variables for task ending times
   * @param productionVars Variables for task productions; represents the amounts of the resource produced by tasks
   * @param consumptionVars Variables for task consumptions; represents the amounts of the resource consumed by tasks
   * @param minCapacity The minimal capacity of the reservoir
   * @param maxCapacity The maximal capacity of the reservoir
   * @param initialAmount The initial amount of resource in the reservoir
   */
  def apply(startVars: Array[CPIntVar], durationVars: Array[CPIntVar], endVars: Array[CPIntVar], productionVars: Array[CPIntVar], consumptionVars: Array[CPIntVar], minCapacity: Int, maxCapacity: Int, initialAmount: Int = 0) =
    new ReservoirResource(startVars,durationVars, endVars, productionVars, consumptionVars, minCapacity, maxCapacity, initialAmount)
}