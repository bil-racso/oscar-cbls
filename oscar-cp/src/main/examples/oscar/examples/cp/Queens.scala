package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object Queens extends CPModel {

  val nQueens = 12 // Number of queens
  val Queens = 0 until nQueens

  // Variables
  val queens = Array.fill(nQueens)(CPVarInt(Queens))

  var nSols = 0
  onSolution { nSols += 1 }

  // Constraints
  add(allDifferent(queens))
  add(allDifferent(Queens.map(i => queens(i) + i)))
  add(allDifferent(Queens.map(i => queens(i) - i)))

  // Search heuristic
  solve search binaryFirstFail(queens)
  
  // Execution
  val stats = start()

  println(s"#sol: $nSols")
  println(stats)
}
