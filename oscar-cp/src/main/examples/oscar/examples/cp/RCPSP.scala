package oscar.examples.cp

import oscar.cp._

object RCPSP extends CPModel with App {
  
  // (duration, consumption)
  val instance = Array((5, 1), (3, 1), (9, 3), (1, 2), (2, 2), (8, 1), (3, 2), (2, 2), (2, 1), (1, 1), (1, 2), (2, 2), (8, 1))
  val durationsData = instance.map(_._1)
  val demandsData = instance.map(_._2)
  val capa = 4
  val horizon = 18//durationsData.sum
  val nTasks = durationsData.length

  solver.silent = true

  val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntVar(0, horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durationsData(t))
  val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)))
  val makespan = maximum(ends)
  
  add(maxCumulativeResource(starts, durations, ends, demands, CPIntVar(capa)), Weak)
  
  minimize(makespan) 
  
  search {
    binaryStatic(starts)//, valHeuris)(starts, durations, ends)
  }
  
  onSolution { 
    println(makespan.value)
    println(starts.mkString("\n"))
  }
    
  val stats = start()
  
  println(stats)
}