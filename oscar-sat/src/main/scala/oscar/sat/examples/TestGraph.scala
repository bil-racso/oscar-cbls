package oscar.sat.examples

import oscar.sat.utils.DimacsFile
import oscar.sat.heuristics.ActivityHeuristic

object TestGraph extends App {

  // Data
  val instance = DimacsFile.parse("data/sat/dimacs/Bejing/enddr2-10-by-5-1.cnf")
  val solver = instance.model
  val heuristic = new ActivityHeuristic(instance.nVariables, solver)
  
  // Solve
  val t0 = System.nanoTime()
  val solved =  solver.solve(heuristic)
  val t1 = System.nanoTime() - t0
  
  // Out
  val solution = solver.solution
  println("satisfiable : " + solved)
  println("verified    : " + instance.verify(solution))
  println("#variables  : " + instance.nVariables)
  println("#clauses    : " + instance.nClauses)
  println("#conflicts  : " + solver.totalConfict)
  println("time (ms)   : " + (t1 / 1000000))
}