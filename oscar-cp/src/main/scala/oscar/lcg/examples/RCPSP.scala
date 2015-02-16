package oscar.lcg.examples

import oscar.lcg._
import oscar.lcg.constraints.DecompTT
import oscar.lcg.core.LCGSearch
import oscar.lcg.heuristic.StaticMinHeuristic

object RCPSP extends LCGModel with App {

  // Data
  val scale = 1
  val instance = Array((5, 1), (3, 1), (9, 3), (1, 2), (2, 2), (8, 1), (3, 2), (2, 2), (2, 1), (1, 1), (1, 2), (2, 2), (8, 1))
  
  //val instance = Array((3, 1), (3, 2), (3, 3), (2, 2), (1, 2))
  val durations = instance.map(_._1 * scale)
  val demands = instance.map(_._2)
  val horizon = 19 * scale//durations.sum
  val nTasks = durations.length
  val capa = 4

  // Decision variables
  val starts = Array.tabulate(nTasks)(t => LCGIntervalVar(0, horizon - durations(t), "Task_" + (t + 1)))

  // LCG Constraints
  val constraint = new DecompTT(starts, durations, demands, capa, horizon)
  cpSolver.add(constraint)

  val search = new LCGSearch(cpSolver, lcgStore)
  val heuristic = new StaticMinHeuristic(starts)
  
  search.onSolution {
    println("\nSOLUTION\n" + starts.map(v => v.name + " = " + v.min).mkString("\n"))
    println("Valid: " + SolutionChecker.check(starts.map(_.min), durations, demands, capa, horizon))
  }
  
  val t0 = System.currentTimeMillis()
  search.search(heuristic, () => search.nSolutions == 1)
  println("\ntime (ms)  : " + (System.currentTimeMillis() - t0))
  println("nConflicts : " + search.nConflicts)
  println("nNodes     : " + search.nNodes)
  println("nLearnt    : " + lcgStore.nLeanrt)
  println("nSolutions : " + search.nSolutions)
}