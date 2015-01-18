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

import oscar.cp._
import oscar.algo.search.Branching

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * @author Pierre Schaus pschaus@gmail.com
 */
object QueensJC {
  def main(args: Array[String]) {

    val cp = CPSolver()
    cp.silent = true
    val n = 88 //number of queens
    val Queens = 0 until n
    //variables
    val queens = for (i <- Queens) yield CPIntVar(cp, 1 to n)

    var nbsol = 0

    val cl = Weak
    cp.add(allDifferent(queens), Weak)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) + i), Weak)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) - i), Weak)
    
    cp.search {
      binaryFirstFail(queens)
    }
    
    println(cp.start(nSols=1))

  }
}


