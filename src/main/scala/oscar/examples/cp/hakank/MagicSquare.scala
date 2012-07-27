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
import scala.math._

/**

  Magic square in Oscar.


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/

 */
object MagicSquare {

 
  def main(args: Array[String]) {

    val cp = CPSolver()

    var n = 4
    if (args.length > 0) {
      n = args(0).toInt
    }
    val n2 = n*n

    println("n:" + n)

    //
    // variables
    //
    val x = Array.fill(n,n)(CPVarInt(cp, 1 to n2))

    // val total = CPVarInt(cp, 1 to n*n*n)
    val total = (n * (n*n + 1) / 2)

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

       cp.add(alldifferent(x.flatten))

       // rows and columns
       for(i <- 0 until n) {
         cp.add(sum( Array.tabulate(n)(j=> x(i)(j)) ) == total)
         cp.add(sum( Array.tabulate(n)(j=> x(j)(i)) ) == total)
       }
  
       // diagonals
       cp.add(sum( Array.tabulate(n)(i=> x(i)(i)) ) == total)
       cp.add(sum( Array.tabulate(n)(i=> x(i)(n-i-1)) ) == total)

       // symmetry breaking
       cp.add(x(0)(0)   < x(0)(n-1))
       cp.add(x(0)(n-1) < x(n-1)(0))
       cp.add(x(0)(0)   < x(n-1)(n-1))


     } exploration {
       
      cp.binaryFirstFail(x.flatten)

       println("\nSolution:\ntotal " + total)
       for(i <- 0 until n) {
         for(j <- 0 until n) {
           print(x(i)(j))
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
