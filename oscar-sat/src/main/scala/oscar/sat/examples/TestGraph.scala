package oscar.sat.examples

import oscar.sat.utils.DimacsFile
import oscar.sat.heuristics.ActivityHeuristic

object TestGraph extends App {

  // Data
  val instance = DimacsFile.parse("data/sat/dimacs/queens10.cnf")
  val solver = instance.model
  val heuristic = new ActivityHeuristic(instance.nVariables, solver)

  // Solve
  val t0 = System.nanoTime()
  val solved = solver.solve(heuristic)
  val t1 = System.nanoTime() - t0

  // Out
  val solution = solver.solution
  println("satisfiable : " + solved)
  println("verified    : " + instance.verify(solution))
  println("#variables  : " + instance.nVariables)
  println("#clauses    : " + instance.nClauses)
  println("#conflicts  : " + solver.totalConfict)
  println("time (ms)   : " + (t1 / 1000000))
  
  var nSols = 0

  while (solver.solve(heuristic)) {
    
    nSols += 1

    for (j <- 0 to 90 by 10) {
      for (i <- 0 to 9) {
        if (solver.solution(j + i)) print("X ") else print(". ")
      }
      println()
    }

    println()
    println("--------------------")
    println()

    solver.newClause(Array.tabulate(100)(i => {
      if (solver.solution(i)) i * 2 + 1
      else i * 2
    }))
  }
  
  println("#conflicts  : " + solver.totalConfict)
  println(nSols)
}