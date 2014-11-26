/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.examples.cbls.queens

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.modeling._

import scala.util.Random

/** Local Search for NQueens
 *  Moves are operated by swapping variables, using a standard neighborhood
 */
object NQueensEasy1 extends CBLSModel with App{

  val N = 20

  println("NQueenEasy(" + N + ")")
  val range:Range = Range(0,N)

  val init = Random.shuffle(range.toList).iterator
  val queens = Array.tabulate(N)((q:Int) => CBLSIntVar(0 to N-1,init.next(), "queen" + q))

  //c.post(AllDiff(Queens)) //enforced because we swap queens and they are always allDiff
  post(allDiff(for (q <- range) yield (queens(q) + q).toIntVar))
  post(allDiff(for (q <- range) yield (q - queens(q)).toIntVar))

  val maxViolQueens = argMax(violations(queens)).toSetVar("most violated queens")

  close()

  val neighborhood =
    swapsNeighborhood(queens, c.violation, "SwapQueens",
      searchZone2 = maxViolQueens, //much faster than using searchZone1, since hotRestart is on Zone1, and not on zone2, and would be log(n)
      symmetryCanBeBrokenOnIndices = false) //since one search zone has been specified, and the other is the full range

  val it = neighborhood.doAllMoves(_ >= N || c.violation.value == 0)

  println("it: " + it)
  println(queens.mkString(","))
}
