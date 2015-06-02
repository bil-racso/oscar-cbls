package oscar.sat.examples

import oscar.sat.utils.DimacsFile

object TestGraph extends App {

  // Data
  val instance = DimacsFile.parse("data/sat/dimacs/graphcoloring/flat200-479/flat200-10.cnf")
  val solver = instance.model
  
  // Solve
  val t0 = System.nanoTime()
  val solved =  solver.solve
  val t1 = System.nanoTime() - t0
  
  // Out
  val solution = solver.solution
  println("satisfiable : " + solved)
  println("verified    : " + instance.verify(solution))
  println("#variables  : " + solution.length)
  println("time (ms)   : " + (t1 / 1000000))
}