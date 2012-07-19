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
import scala.io.Source._
import scala.math._

/**
 *
 * Sudoku solver in Oscar.
 *
 * See http://en.wikipedia.org/wiki/Sudoku
 *
 * This version can also read a problem instance from a file.
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Sudoku2 extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    var n = 9
    var reg = 3
    val X = 0

    // 
    // data
    //
    // This problem is problem 0 from
    // Gecode's sudoku.cpp
    // http://www.gecode.org/gecode-doc-latest/sudoku_8cpp-source.html
    //
    var problem = List(List(X, X, X, 2, X, 5, X, X, X),
                       List(X, 9, X, X, X, X, 7, 3, X),
                       List(X, X, 2, X, X, 9, X, 6, X),
                       List(2, X, X, X, X, X, 4, X, 9),
                       List(X, X, X, X, 7, X, X, X, X),
                       List(6, X, 9, X, X, X, X, X, 1),
                       List(X, 8, X, 4, X, X, 1, X, X),
                       List(X, 6, 3, X, X, X, X, 8, X),
                       List(X, X, X, 6, X, 8, X, X, X))

     
  // read problem instance from file
  if (args.length > 0) {
    println("\nReading from file: " + args(0))
    
    val lines = fromFile(args(0)).getLines.filter(!_.startsWith("#")).toList
    n = lines(0).toInt
    reg = sqrt(n).toInt
    println("Size:" + n)
    var this_problem = List[List[Int]]()
    for (t <- 1 to n) {
      println(lines(t))
      val t2: List[Int] = lines(t).split("\\s+").toList.map(i=>if (i == ".") X else i.toInt)
      this_problem = this_problem ::: List(t2)
    }
    println()

    problem = this_problem

  }


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
         println(x(i).mkString(""))
       }
       println()

       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
