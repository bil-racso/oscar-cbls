package oscar.examples.cp

import oscar.cp._

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 * 
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object Queens extends CPModel with App {

  val nQueens = 1000 // Number of queens
  val Queens  = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPIntVar(Queens))

  // Constraints
  add(allDifferent(queens))
  add(allDifferent(Queens.map(i => queens(i) + i)))
  add(allDifferent(Queens.map(i => queens(i) - i)))
  
  val array = Array.tabulate(10)(i => i)
  
  val c = array(queens(0))

  // Search heuristic
  search(binaryFirstFail(queens))
  
  // Execution
  val stats = start(nSols = 1)

  println(stats)
}
