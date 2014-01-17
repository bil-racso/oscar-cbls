package oscar.examples.cp.scheduling

import oscar.cp.core._
import oscar.cp.modeling._

object OptionalTasks extends App {

  val horizon = 8
  val durationsData = Array(2, 2, 3, 3, 3, 4, 4, 5, 6, 7)
  val profitsData   = Array(1, 2, 4, 3, 2, 5, 3, 6, 4, 8)
  val demandsData   = Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  val capaMax = 2

  val nTasks = durationsData.size
  val Tasks = 0 until nTasks

  implicit val cp = CPSolver()
  cp.silent = true
  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => CPIntVar(durations(t).min to horizon))
  val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)))
  val resources = Array.fill(nTasks)(CPIntVar(0 to 1))

  val profits = Array.tabulate(nTasks)(t => resources(t) * profitsData(t))
  val totalProfit = sum(profits)
  
  cp.onSolution {
    println("Total profit of " + totalProfit.value)
    for (t <- Tasks) {
      if (resources(t).value == 1) println("Task " + t + " of profit " + profitsData(t) + " and duration " + durationsData(t) + " is executed at time " + starts(t).value)
    }
  }

  cp.maximize(totalProfit) subjectTo {
    // Consistency 
    for (t <- Tasks) {
      cp.add(ends(t) == starts(t) + durations(t))
    }
    // Cumulative
    cp.add(maxCumulativeResource(starts, durations, ends, demands, resources, CPIntVar(capaMax), 1))

  } search {
   binaryFirstFail(resources) ++  binaryFirstFail(starts)
  }
  println(cp.start())
  
  
}
