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
package oscar.cbls.scheduling.algo

import oscar.cbls.invariants.core.computation.Solution
import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.scheduling.model.Activity
import oscar.cbls.scheduling.model.Planning
import oscar.cbls.scheduling.model.PrecedenceCleaner
import oscar.cbls.scheduling.model.Resource
import oscar.cbls.search.SearchEngine

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

/**
 * @param p
 * @param verbose
 * @author renaud.delandtsheer@cetic.be
 */
class IFlatIRelax(p: Planning, verbose: Boolean = true) extends SearchEngine {
  val model: Store = p.model

  /**
   * This solves the jobshop by iterative relaxation and flattening
   * @param maxIt the max number of iterations of the search
   * @param stable the number of no successive noimprove that will cause the search to stop
   */
  def solve(maxIt: Int,
            stable: Int,
            nbRelax: Int = 4,
            pkillPerRelax: Int = 50) {

    var it: Int = 0

    flattenWorseFirst()

    var bestSolution: Solution = model.solution(true)
    if (verbose) {
      println("----------------")
    }
    p.updateVisual()

    var plateaulength = 0
    var bestMakeSpan = p.makeSpan.value

    if (verbose) println("Initial best make span: " + bestMakeSpan)

    while (it < maxIt && plateaulength < stable) {
      //iterative weakening and flattening
      it += 1

      if (plateaulength > 10 && (plateaulength % 50) == 0) {

        for (i <- 0 until nbRelax * 3) { relax(pkillPerRelax); }
        if (verbose) println("jumping****************")

      } else {
        val m = p.makeSpan.value
        if (!relaxUntilMakespanReduced(pkillPerRelax, nbRelax)) {
          println("STOP criterion: no relaxation could be achieved.")
          return
        }
        if (p.makeSpan.value == m) println("skip")
      }

      flattenWorseFirst()

      if (verbose) {
        println(p.makeSpan)
        println("iteration: " + it)
      }

      if (p.makeSpan.value < bestMakeSpan) {
        bestSolution = model.solution(true)
        bestMakeSpan = p.makeSpan.value
        plateaulength = 0
        if (verbose) println("Better MakeSpan found")
        p.updateVisual()
      } else {
        plateaulength += 1
        p.clean()
      }

      if (verbose) println("----------------")
    }

    if (verbose) {
      if (it >= maxIt)
        println("STOP criterion: maximum iteration number reached.")
      if (plateaulength >= stable)
        println("STOP criterion: " + stable + " iterations without improvement.")
    }

    model.restoreSolution(bestSolution)

    p.clean()

    if (verbose) println("restored best solution")

    p.updateVisual()
  }

  /**
   * performs the relaxation of the critical path
   * @param pKill: the probability to kill a killable precedence constraint in percent
   * @return true if something could be relaxed, false if makespan is solid (made only of dependencies that cannot be relaxed)
   */
  def relax(pKill: Int): Boolean = {

    val potentiallykilledNodes = CriticalPathFinder.nonSolidCriticalPath(p)
    if (potentiallykilledNodes.isEmpty) return false

    for ((from, to) <- potentiallykilledNodes) {
      if (flip(pKill)) {
        to.removeDynamicPredecessor(from, verbose)
      }
    }
    true
  }

  /**
   * performs the relaxation of the critical path
   * @param pKill: the probability to kill a killable precedence constraint in percent
   * @param min: the minimal number of relaxation
   * @return true if something could be relaxed, false if makespan is solid
   */
  def relaxUntilMakespanReduced(pKill: Int, min: Int = 3): Boolean = {
    val m = p.makeSpan.value
    var n = 0
    var SomethingCouldBeRelaxed = false
    while ((p.makeSpan.value == m) || (n < min)) {
      n += 1
      SomethingCouldBeRelaxed = relax(pKill) || SomethingCouldBeRelaxed
      if (!SomethingCouldBeRelaxed) return false
    }
    if (verbose) println("relaxed " + n + " times to shorten makespan")
    return SomethingCouldBeRelaxed
  }

  /**implements the standard flatten procedure*/
  def flattenWorseFirst() {
    var iterations = 0
    while (!p.worseOvershotResource.value.isEmpty) {
      if (iterations > p.activityCount)
        throw new IllegalStateException("FlattenWorseFirst() will not terminate. Check there is no conflict between non moveable activities.")
      iterations += 1

      val r: Resource = p.resourceArray(selectFrom(p.worseOvershotResource.value))

      val t: Int = r.worseOverShootTime

      val conflictActivities = r.conflictingActivities(t)
      val baseForEjection = r.baseActivityForEjection(t)

      selectMax2(baseForEjection, conflictActivities,
        (a: Activity, b: Activity) => (b.latestStartDate.value - a.earliestEndDate.value),
        (a: Activity, b: Activity) => p.canAddPrecedenceAssumingResourceConflict(a, b)) match {
          case (a, b) =>
            b.addDynamicPredecessor(a, verbose)
          case null =>

            //no precedence can be added because some additional precedence must be killed to allow that
            //this happens when superTasks are used, and when dependencies have been added around the start and end tasks of a superTask
            //we search which dependency can be killed in the conflict set,
            val conflictActivityArray = conflictActivities.toArray
            val baseForEjectionArray = baseForEjection.toArray

            val dependencyKillers: Array[Array[PrecedenceCleaner]] =
              Array.tabulate(baseForEjection.size)(
                t1 => Array.tabulate(conflictActivityArray.size)(
                  t2 => p.getDependencyToKillToAvoidCycle(baseForEjectionArray(t1), conflictActivityArray(t2))))

            selectMax2(baseForEjectionArray.indices, conflictActivityArray.indices,
              (a: Int, b: Int) => (conflictActivityArray(b).latestStartDate.value - baseForEjectionArray(a).earliestEndDate.value),
              (a: Int, b: Int) => dependencyKillers(a)(b).canBeKilled) match {
                case (a, b) => {
                  println("need to kill dependencies to complete flattening")
                  dependencyKillers(a)(b).killDependencies(verbose)

                  conflictActivityArray(b).addDynamicPredecessor(baseForEjectionArray(a), verbose)
                }
                case null => throw new Error("cannot flatten at time " + t + " activities: " + conflictActivities)
              }

        }
    }
  }
}


