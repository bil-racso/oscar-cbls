package oscar.cp.scheduling.search

import oscar.cp.testUtils._
import oscar.cp._
import scala.util.Random
import oscar.algo.search.Branching

/**
 *  @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class SetTimesBranchingSuite extends SchedulingSearchSuite(seed = 0, scalable = true) {
  override def searchHeuristic(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], tieBreaker: Int => Int): Branching = {
    setTimes(starts, durations, ends, tieBreaker)
  }
}

class NewSetTimesSuite extends SchedulingSearchSuite(seed = 0, scalable = true) {
  override def searchHeuristic(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], tieBreaker: Int => Int): Branching = {
    new NewSetTimes(starts, ends, tieBreaker)
  }
}

abstract class SchedulingSearchSuite(seed: Int, scalable: Boolean) extends TestSuite {

  /** To implement with the search heuristic to be tested */
  def searchHeuristic(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], tieBreaker: Int => Int): Branching

  private[this] val rng = new Random(seed)

  private def splitRectangle(leftBound: Int, rightBound: Int, minWidth: Int, remainingSplits: Int): List[(Int, Int)] = {
    if (remainingSplits == 0 || (rightBound - leftBound) < 2 * minWidth) List((leftBound, rightBound))
    else {
      val minR = leftBound + minWidth
      val randomSplit = minR + rng.nextInt(rightBound - minR)
      splitRectangle(leftBound, randomSplit, minWidth, remainingSplits - 1) ::: splitRectangle(randomSplit, rightBound, minWidth, remainingSplits - 1)
    }
  }

  test("SetTimes test on a dense rectangle of height 4 and width 100") {

    val minWidth = 10
    val optimalMakespan = 100
    val capacity = 4
    val maxRecursiveSplits = 5

    for (i <- 1 to 10) new CPModel {
      val activitySolution = Array.tabulate(capacity)(i => splitRectangle(0, optimalMakespan, minWidth, maxRecursiveSplits)).flatten
      val nActivities = activitySolution.length
      val durations = activitySolution.map(a => a._2 - a._1)

      solver.silent = true
      val startVars = Array.tabulate(nActivities)(i => CPIntVar(0 to optimalMakespan - durations(i)))
      val endVars = Array.tabulate(nActivities)(i => startVars(i) + durations(i))
      val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i)))
      val demandVars = Array.fill(nActivities)(CPIntVar(1))
      val makespan = maximum(endVars)

      add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)))

      minimize(makespan)
      search { searchHeuristic(startVars, durationVars, endVars, i => -endVars(i).min) }

      var bestSol = 0
      onSolution { bestSol = makespan.value }

      start()

      assert(bestSol == optimalMakespan)
    }
  }

  test("SetTimes test on a dense rectangle of height 10 and width 1000") {

    val minWidth = 10
    val optimalMakespan = 1000
    val capacity = 10
    val maxRecursiveSplits = 10

    for (i <- 1 to 10) new CPModel {

      val activitySolution = Array.tabulate(capacity)(i => splitRectangle(0, optimalMakespan, minWidth, maxRecursiveSplits)).flatten
      val nActivities = activitySolution.length
      val durations = activitySolution.map(a => a._2 - a._1)

      solver.silent = true
      val startVars = Array.tabulate(nActivities)(i => CPIntVar(0 to optimalMakespan - durations(i)))
      val endVars = Array.tabulate(nActivities)(i => startVars(i) + durations(i))
      val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i)))
      val demandVars = Array.fill(nActivities)(CPIntVar(1))
      val makespan = maximum(endVars)

      add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)))

      minimize(makespan)
      search { searchHeuristic(startVars, durationVars, endVars, i => -endVars(i).min) }

      var bestSol = 0
      onSolution { bestSol = makespan.value }

      start()

      assert(bestSol == optimalMakespan)
    }
  }

  test("Steven example") {

    val durations = Array(2, 4, 3, 3)
    val demands = Array(1, 1, 2, 2)
    val horizon = 6
    val capa = 3

    new CPModel {

      val nTasks = durations.length
      val startVars = Array.tabulate(nTasks)(i => CPIntVar(0, horizon - durations(i)))
      val endVars = Array.tabulate(nTasks)(i => startVars(i) + durations(i))
      val durationVars = Array.tabulate(nTasks)(i => CPIntVar(durations(i)))
      val demandVars = Array.tabulate(nTasks)(i => CPIntVar(demands(i)))

      solver.silent = true

      add(startVars(0) == 0)
      add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capa)), Strong)
      search { searchHeuristic(startVars, durationVars, endVars, i => i) }

      val stats = start(nSols = 1)
      assert(stats.nSols == 1)
    }
  }

  test(scalable, "Scaling should not impact the number of nodes.") {

    val scaling1 = 6
    val scaling2 = 100

    val minWidth = 10
    val optimalMakespan = 100
    val capacity = 4
    val maxRecursiveSplits = 5

    for (i <- 1 to 10) {

      // Data
      val activitySolution = Array.tabulate(capacity)(i => splitRectangle(0, optimalMakespan, minWidth, maxRecursiveSplits)).flatten
      val nTasks = activitySolution.length
      val durationsData = activitySolution.map(a => a._2 - a._1)
      println("nTasks " + nTasks)

      var nNodes1 = 0
      var nNodes2 = 0
      var nNodes3 = 0

      new CPModel {
        solver.silent = true
        val horizon = optimalMakespan
        val durations = durationsData
        val startVars = Array.tabulate(nTasks)(i => CPIntVar(0, optimalMakespan - durations(i)))
        val endVars = Array.tabulate(nTasks)(i => startVars(i) + durations(i))
        val durationVars = Array.tabulate(nTasks)(i => CPIntVar(durations(i)))
        val demandVars = Array.fill(nTasks)(CPIntVar(1))
        val makespan = maximum(endVars)

        add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)))

        minimize(makespan)
        search { searchHeuristic(startVars, durationVars, endVars, i => -endVars(i).min) }

        val stats = start()
        nNodes1 = stats.nNodes
      }

      new CPModel {
        solver.silent = true
        val horizon = optimalMakespan * scaling1
        val durations = durationsData.map(_ * scaling1)
        val startVars = Array.tabulate(nTasks)(i => CPIntVar(0, horizon - durations(i)))
        val endVars = Array.tabulate(nTasks)(i => startVars(i) + durations(i))
        val durationVars = Array.tabulate(nTasks)(i => CPIntVar(durations(i)))
        val demandVars = Array.fill(nTasks)(CPIntVar(1))
        val makespan = maximum(endVars)

        add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)))

        minimize(makespan)
        search { searchHeuristic(startVars, durationVars, endVars, i => -endVars(i).min) }

        val stats = start()
        nNodes2 = stats.nNodes
      }

      new CPModel {
        solver.silent = true
        val horizon = optimalMakespan * scaling2
        val durations = durationsData.map(_ * scaling2)
        val startVars = Array.tabulate(nTasks)(i => CPIntVar(0, horizon - durations(i)))
        val endVars = Array.tabulate(nTasks)(i => startVars(i) + durations(i))
        val durationVars = Array.tabulate(nTasks)(i => CPIntVar(durations(i)))
        val demandVars = Array.fill(nTasks)(CPIntVar(1))
        val makespan = maximum(endVars)

        add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)))

        minimize(makespan)
        search { searchHeuristic(startVars, durationVars, endVars, i => -endVars(i).min) }

        val stats = start()
        nNodes3 = stats.nNodes
      }
      
      assert(nNodes1 == nNodes2, s"scaling by $scaling1 increases the number of nodes from $nNodes1 to $nNodes2")
      assert(nNodes1 == nNodes3, s"scaling by $scaling2 increases the number of backtracks from $nNodes1 to $nNodes3")
    }
  }
}