package oscar.examples.cp.scheduling

import oscar.cp.core._
import oscar.cp.modeling._
import oscar.cp.constraints.SweepMaxCumulative

object VariableCapacity extends App {

  val horizon = 8
  val durationsData = Array(2, 2, 3, 3, 3, 4, 4, 5, 6, 7)
  val demandsData   = Array(1, 3, 2, 1, 2, 3, 1, 2, 1, 2)

  val nTasks = durationsData.size
  val Tasks = 0 until nTasks

  val cp = CPSolver()
  cp.silent = true
  val durations = Array.tabulate(nTasks)(t => CPVarInt(cp, durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPVarInt(cp, 0 to horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => CPVarInt(cp, durations(t).min to horizon))
  val demands = Array.tabulate(nTasks)(t => CPVarInt(cp, demandsData(t)))
  val resources = Array.fill(nTasks)(CPVarInt(cp, 1))

  val capacity = CPVarInt(cp, 0 to demandsData.sum)

  cp.minimize(capacity) subjectTo {
    // Consistency 
    for (t <- Tasks) {
      cp.add(ends(t) == starts(t) + durations(t))
    }
    // Cumulative
    cp.add(SweepMaxCumulative(starts, ends, durations, demands, resources, capacity, 1))

  } exploration {
    cp.binaryFirstFail(starts)
    cp.binaryFirstFail(capacity)
    
    println("Total capacity of " + capacity.value)
    for (t <- Tasks) {
      print("Task " + t)
      print(" of duration " + durations(t).value)
      print(" and demand " + demands(t).value)
      println(" is executed at time " + starts(t).value)
    }
    println()
  }

  cp.run()
  cp.printStats
}
