package oscar.cp.lcg

import oscar.cp._
import oscar.cp.lcg._
import oscar.cp.lcg.constraints.DecompChecker
import oscar.cp.lcg.core.LCGSolver
import oscar.cp.lcg.searches.MinMinHeuristic
import oscar.cp.lcg.searches.LCGSearch
import oscar.cp.lcg.constraints.DecompTT

object TestScheduling extends App {

  // Data
  val instance = Array((5, 1), (3, 1), (9, 3), (1, 2), (2, 2), (8, 1), (3, 2), (2, 2), (2, 1), (1, 1), (1, 2), (2, 2), (8, 1))
  val durations = instance.map(_._1)
  val demands = instance.map(_._2)
  val horizon = 19//durations.sum
  val nTasks = durations.length
  val capa = 4
  
  implicit val cpSolver: CPSolver = CPSolver() 
  final val lcgStore: LCGStore = new LCGStore(cpSolver)
  implicit val lcgSolver: LCGSolver = new LCGSolver(cpSolver, lcgStore)

  // Decision variables
  val starts = Array.tabulate(nTasks)(t => LCGIntervalVar(0, horizon - durations(t), "Task_" + (t + 1)))

  // LCG Constraints
  val constraint = new DecompChecker(lcgSolver, starts, durations, demands, capa, horizon)
  cpSolver.add(constraint)

  val search = new LCGSearch(cpSolver, cpSolver, lcgStore)
  val heuristic = new MinMinHeuristic(starts)
  
  search.onSolution {
    println("\nSOLUTION\n" + starts.map(v => v.name + " = " + v.min).mkString("\n"))
  }
  
  val t0 = System.currentTimeMillis()
  search.search(heuristic, () => search.nSolutions == 1)
  println("\ntime (ms)  : " + (System.currentTimeMillis() - t0))
  println("nConflicts : " + search.nConflicts)
  println("nNodes     : " + search.nNodes)
  println("nSolutions : " + search.nSolutions)
}