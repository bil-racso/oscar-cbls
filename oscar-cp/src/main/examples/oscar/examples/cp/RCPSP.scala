package oscar.examples.cp

import oscar.cp._
import oscar.cp.scheduling.constraints.TTPerTask
import oscar.cp.constraints.scheduling.LinearTT

object RCPSP extends CPModel with App {
  
  /*
  test LinearTT instance 4: failed !
  expected number of solutions: 12
  number of solutions: 4
  INSTANCE
  durations: 3, 2
  demands: 3, 0
  resources: 0, 0
  capacity: 4
  horizon: 5
  */
  
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
  
  val resources = Array.fill(nTasks)(CPIntervalVar(0))
  val capacity = CPIntervalVar(capa)
  
  val mirrorStart = ends.map(-_)
  val mirrorEnd = starts.map(-_)
  
  // nNodes: 1445964
  // nFails: 722983
  
  // nNodes: 2570984
  // nFails: 1285493
  
  //add(TTPerTask(starts, durations, ends, demands, resources, capacity, 0))
  
  add(new LinearTT(starts, durationsData, ends, demandsData, capa))
  
  //add(maxCumulativeResource(starts, durations, ends, demands, CPIntervalVar(capa)),Weak)
  
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