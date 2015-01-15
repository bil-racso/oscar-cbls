package oscar.examples.cp

import oscar.cp._

object RCPSP extends CPModel with App {
  
  // (duration, consumption)
  val instance = Array((50, 1), (30, 1), (90, 3), (10, 2), (20, 2), (80, 1), (30, 2), (20, 2), (20, 1), (10, 1), (10, 2), (20, 2), (80, 1))
  val durationsData = instance.map(_._1)
  val demandsData = instance.map(_._2)
  val capa = 4
  val horizon = durationsData.sum
  val nTasks = durationsData.length

  solver.silent = true

  val durations = Array.tabulate(nTasks)(t => CPIntervalVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntervalVar(0, horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durationsData(t))
  val demands = Array.tabulate(nTasks)(t => CPIntervalVar(demandsData(t)))
  val makespan = maximum(ends)
  
  add(maxCumulativeResource(starts, durations, ends, demands, CPIntervalVar(capa)), Weak)
  
  minimize(makespan) 
  
  search {
    setTimes(starts, durations, ends)
  }
  
  onSolution { 
    println(makespan.value)
  }
    
  val stats = start()
  
  println(stats)
}