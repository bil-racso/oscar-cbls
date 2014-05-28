package oscar.cbls.scheduling.algo

import oscar.cbls.invariants.core.computation.Solution
import oscar.cbls.invariants.core.computation.Store
import oscar.cbls.scheduling.model.Activity
import oscar.cbls.scheduling.model.Planning
import oscar.cbls.scheduling.model.PrecedenceCleaner
import oscar.cbls.scheduling.model.Resource
import oscar.cbls.search.SearchEngine
import oscar.cbls.scheduling.model.Deadlines
import scala.util.control.Breaks.break
import oscar.cbls.scheduling.model.ActivityWithDeadline
import CriticalPathFinder.nonSolidCriticalPath
import oscar.cbls.scheduling.model.TotalResourcesOvershootEvaluation
import oscar.cbls.scheduling.model.ActivityWithDeadline
import oscar.cbls.scheduling.model.NonMoveableActivity
import oscar.cbls.routing.model.TotalTimeSpentByVehiclesOutOfDepotAsObjectiveTerm
import oscar.cbls.scheduling.model.ActivityWithDeadline
import oscar.cbls.scheduling.model.VariableResources

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

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Yoann Guyot
 * ****************************************************************************
 */

/**
 * @param planning
 * @param maxLocalIterations
 * @param temperature Metropolis parameter
 * @param verbose
 * @author yoann.guyot@cetic.be
 */
class TardinessSearch(planning: Planning with Deadlines with TotalResourcesOvershootEvaluation with VariableResources,
                      temperature: Float = 100,
                      verbose: Boolean = false) extends SearchEngine {
  val model: Store = planning.model

  require(model.isClosed, "model should be closed before TardinessSearch algo can be instantiated")

  var minOvershootValue: Int = 0
  var bestSolution: Solution = null

  /**
   * This solves the jobshop by neighborhood exploration. This is done by applying
   * movements in a set of critical activities.
   * @param maxTrials the max number of iterations of the search
   * @param stable the max number of successive iterations with no improvement
   */
  def solve(maxTrials: Int,
            stable: Int,
            maxLocalIterations: Int = 5,
            onlyImprovingMoves: Boolean = true,
            saveCurrentSolution: Boolean = false) = {
    var hasImproved = false
    if (planning.totalTardiness.value > 0) {
      if (saveCurrentSolution) {
        bestSolution = model.solution(true)
        minOvershootValue = planning.totalOvershoot.value
      }
      var nbTrials: Int = 0
      var change: Boolean = true

      while (nbTrials < maxTrials) {
        if (verbose) println("Tardiness search trial " + nbTrials + ".")

        var precCriticalActivities: List[(Activity, Activity)] = List()
        for (activity <- planning.activitiesWithDeadlines.filter(_.isLate)) {
          if (verbose) println("Activity " + activity.name + " is late.")
          // a list of (predecessor, activity) with an additional tight dependence
          val criticalActivities = nonSolidCriticalPath(planning)(activity)
          // if (exploreNeighborhood(criticalActivities, maxLocalIterations))
          if (!criticalActivities.isEmpty
            && criticalActivities != precCriticalActivities) {
            if (exploreNeighborhood(criticalActivities,
              maxLocalIterations, onlyImprovingMoves))
              hasImproved = true
          } else {
            if (verbose) println("Skip (nothing to do).\n")
          }
          precCriticalActivities = criticalActivities
        }
        nbTrials = nbTrials + 1
      }

      if (bestSolution != null) model.restoreSolution(bestSolution)
      planning.clean()
      if (verbose) println("Restored best solution.")
    }
    hasImproved
  }

  // a list of (predecessor, activity) with an additional tight dependence
  private def exploreNeighborhood(criticals: List[(Activity, Activity)],
                                  maxLocalIterations: Int,
                                  onlyImproving: Boolean = true) = {
    var hasImproved = false
    var moved = false
    var i = 0

    var criticalsIterator = criticals.iterator

    while (!moved
      && (if (maxLocalIterations > 0) i < maxLocalIterations else true)
      && criticalsIterator.hasNext) {

      if (verbose) println("Exploration iteration " + i + ".")
      moved = false
      val (from, to) = criticalsIterator.next

      if (verbose) {
        println("tardiness: " + planning.totalTardiness)
      }
      val gain = swap(from, to)
      if (verbose) {
        println("tardiness: " + planning.totalTardiness)
      }

      if (verbose) println("gain = " + gain)

      if (gain <= 0) {
        val currentOvershoot = planning.totalOvershoot.value
        if (verbose) println("overshoot = " + minOvershootValue + " -> " + currentOvershoot)
        if (currentOvershoot <= minOvershootValue) {
          minOvershootValue = currentOvershoot
          bestSolution = model.solution(true)
          hasImproved = true
          if (verbose) println("(improvement) Swapped " + from + " with " + to + "\n")
          moved = true
        } else if (!onlyImproving && math.random < math.exp(-gain / temperature)) {
          if (verbose) println("(random " + temperature + "°) Swapped "
            + from + " with " + to + "\n")
          moved = true
        } else {
          // cancel move
          swap(to, from)
          if (verbose) println("No move (Swap undone).\n")
        }
        // metropolis criterion
      } else if (!onlyImproving && math.random < math.exp(-gain / temperature)) {
        if (verbose) println("(random " + temperature + "°) Swapped "
          + from + " with " + to + "\n")
        moved = true
      } else {
        // cancel move
        swap(to, from)
        if (verbose) println("No move (Swap undone).\n")
      }

      i = i + 1
    }

    hasImproved
  }

  private def swap(from: Activity, to: Activity): Int = {
    val previousTardiness = planning.totalTardiness.value
    if (to.canAddPrecedence) {
      to.removeDynamicPredecessor(from, verbose)

//      val predecessors = from.potentiallyKilledPredecessors.value.toList.map(
//        planning.activityArray(_))
//      println("PREDECESSORS: " + predecessors)
//      val restrictors = predecessors.filter(planning.resourceRestrictionTasks.contains(_))
//      println("RESTRICTORS: " + restrictors)
//      restrictors.foreach(to.addDynamicPredecessor(_, verbose))

      if (from.canAddPrecedence)
        from.addDynamicPredecessor(to, verbose)

      val currentTardiness = planning.totalTardiness.value
      currentTardiness - previousTardiness
    } else {
      -1
    }
  }
}
