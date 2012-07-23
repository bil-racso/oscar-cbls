/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.math.pow

/**
 *
 * Sudoku solver in Oscar.
 * 
 * See http://en.wikipedia.org/wiki/Sudoku
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Sudoku extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val n = 9
    val reg = 3

    // 
    // data
    //
    // This problem is problem 0 from
    // Gecode's sudoku.cpp
    // http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
    //
    val problem = List(List(0, 0, 0, 2, 0, 5, 0, 0, 0),
                       List(0, 9, 0, 0, 0, 0, 7, 3, 0),
                       List(0, 0, 2, 0, 0, 9, 0, 6, 0),
                       List(2, 0, 0, 0, 0, 0, 4, 0, 9),
                       List(0, 0, 0, 0, 7, 0, 0, 0, 0),
                       List(6, 0, 9, 0, 0, 0, 0, 0, 1),
                       List(0, 8, 0, 4, 0, 0, 1, 0, 0),
                       List(0, 6, 3, 0, 0, 0, 0, 8, 0),
                       List(0, 0, 0, 6, 0, 8, 0, 0, 0))

    // variables
    val x = Array.fill(n,n)(CPVarInt(cp, 1 to n))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      // fill with the hints
      for(i <- 0 until n;
          j <- 0 until n if problem(i)(j) > 0) {
            cp.add(x(i)(j) == problem(i)(j))
      }
      
      // rows and columns
      for(i <- 0 until n) {
        cp.add(alldifferent( Array.tabulate(n)(j=> x(i)(j))), Strong)
        cp.add(alldifferent( Array.tabulate(n)(j=> x(j)(i))), Strong)
      }
      
      // blocks
      for(i <- 0 until reg; j <- 0 until reg) {
        cp.add(alldifferent(  (for{ r <- i*reg until i*reg+reg;
                                    c <- j*reg until j*reg+reg
              } yield x(r)(c)).toArray), Strong)
      }
      

    } exploration {
       
       cp.binaryFirstFail(x.flatten)

       println("\nSolution:")
       for(i <- 0 until n) {
         for(j <- 0 until n) {
           print(x(i)(j) + " ")
         }
         println()
       }
       println()

       numSols += 1
       
    }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
