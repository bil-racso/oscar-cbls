package oscar.examples.cp

import oscar.cp._
import oscar.algo.search.NewSearch
import oscar.algo.reversible.ReversibleBoolean
import oscar.algo.search.NewSearch

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object Queens extends CPModel with App {

  val nQueens = 88 // Number of queens
  val Queens = 0 until nQueens

  // Variables
  //val queens = Array.fill(nQueens)(CPIntVar.sparse(0, nQueens - 1))
  val queens = Array.fill(nQueens)(CPIntVar.mySparse(0, nQueens - 1))
  val rQueens = Array.tabulate(nQueens)(i => queens(i) + i)
  val fQueens = Array.tabulate(nQueens)(i => queens(i) - i)

  for (i <- Queens; j <- Queens; if j < i) {
    add(queens(i) != queens(j))
    add(rQueens(i) != rQueens(j))
    add(fQueens(i) != fQueens(j))
  }

  val t0 = System.currentTimeMillis()
  val search = new NewSearch(solver)
  val heuristic = binaryFirstFail(queens)
  search.start(heuristic, _.nSolutions == 1)
  val time = System.currentTimeMillis() - t0
  println("time " + time)
  
  // Constraints
  //add(allDifferent(queens))
  //add(allDifferent(Queens.map(i => queens(i) + i)))
  //add(allDifferent(Queens.map(i => queens(i) - i)))


  // Search heuristic
  /*search(binaryFirstFail(queens))

  // Execution
  val stats = start(nSols = 1)
  println(stats)*/
}
