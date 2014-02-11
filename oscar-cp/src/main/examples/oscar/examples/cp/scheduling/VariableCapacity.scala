package oscar.examples.cp.scheduling

import oscar.cp.core._
import oscar.cp.modeling._

object VariableCapacity extends CPModel with App {

  val horizon = 8
  val durationsData = Array(2, 2, 3, 3, 3, 4, 4, 5, 6, 7)
  val demandsData = Array(1, 3, 2, 1, 2, 3, 1, 2, 1, 2)

  val nTasks = durationsData.size
  val Tasks = 0 until nTasks

  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => CPIntVar(durations(t).min to horizon))
  val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)))
  val resources = Array.fill(nTasks)(CPIntVar(1))

  val capacity = CPIntVar(0 to demandsData.sum)

  // Consistency 
  for (t <- Tasks) {
    add(ends(t) == starts(t) + durations(t))
  }
  // Cumulative
  add(maxCumulativeResource(starts, durations, ends, demands, resources, capacity, 1))

  minimize(capacity) search {
    binaryFirstFail(starts) ++ binaryFirstFail(Seq(capacity))
  }

  onSolution {
    println("Total capacity of " + capacity.value)
    for (t <- Tasks) {
      print("Task " + t)
      print(" of duration " + durations(t).value)
      print(" and demand " + demands(t).value)
      println(" is executed at time " + starts(t).value)
    }
    println()
  }

  println(start())
}
