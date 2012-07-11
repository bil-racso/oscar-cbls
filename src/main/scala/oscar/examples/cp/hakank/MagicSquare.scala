/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
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
object MagicSquare extends CPModel {

 
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
    val x = Array.fill(n)(Array.fill(n)(CPVarInt(cp, 1 to n2)))

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
