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


/**

  Sequence sum in Oscar.

  Sum of each sequence in s-slices in an array of n elements should be m.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/

 */
object SequenceSum extends CPModel {

  // Sum the elements in y where each subsequence of length s
  // sums to m
  def sequence_sum(cp: CPSolver, y: Array[CPVarInt], m: CPVarInt, s: Int) = {
    val n = y.length
    for(i <- 0 until n - s + 1) {
      cp.add(sum( Range(i,i+s).map(j => y(j) ).toList) == m)
    }
    
  }

 
  def main(args: Array[String]) {

    val cp = CPSolver()


    val n = 6
    // val m = 10 // the sum
    val s = 3 // the sliding size


    // variables
    val x = Array.fill(n)(CPVarInt(cp, 1 to n))
    // the sum
    val m = CPVarInt(cp, 1 to n*n)


    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

        // cp.(alldifferent(x), Strong)
  
        sequence_sum(cp, x, m, s)
        cp.add(m == 10)

        // symmetry breaking
        // x(0) #= 1


     } exploration {
       
       cp.binaryFirstFail(x)

       print("x: " + x.mkString(""))
       println("  m: " + m)


       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
