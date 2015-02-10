package oscar.cp.lcg

import oscar.cp._
import oscar.cp.lcg._
import oscar.cp.lcg.constraints.DecompChecker
import oscar.cp.lcg.core.LCGSolver
import oscar.cp.lcg.searches.MinMinHeuristic
import oscar.cp.lcg.searches.LCGSearch

object TestScheduling extends App {

  // Data
  val instance = Array((50, 1), (30, 1), (90, 3), (10, 2), (20, 2), (80, 1), (30, 2), (20, 2), (20, 1), (10, 1), (10, 2), (20, 2), (80, 1))
  val durations = instance.map(_._1)
  val demands = instance.map(_._2)
  val horizon = durations.sum
  val nTasks = durations.length
  val capa = 4
  
  implicit val cpSolver: CPSolver = CPSolver() 
  final val lcgStore: LCGStore = new LCGStore(cpSolver)
  implicit val lcgSolver: LCGSolver = new LCGSolver(cpSolver, lcgStore)

  // Decision variables
  val starts = Array.tabulate(nTasks)(t => LCGIntervalVar(0, horizon - durations(t), "Task_" + t))

  // LCG Constraints
  cpSolver.add(new DecompChecker(lcgSolver, starts, durations, demands, capa, horizon))

  val search = new LCGSearch(cpSolver, cpSolver, lcgStore)
  val heuristic = new MinMinHeuristic(starts)
  
  //search.onFailure(println("backjump"))
  search.onSolution(println("\nSOLUTION\n" + starts.map(v => v.name + " = " + v.min).mkString("\n")))
  val t0 = System.currentTimeMillis()
  search.search(heuristic, () => search.nSolutions == 1)
  println("time (ms)  : " + (System.currentTimeMillis() - t0))
  println("nBackjumps : " + search.nBacktracks)
  println("nNodes     : " + search.nNodes)
  println("nSolutions : " + search.nSolutions)
}