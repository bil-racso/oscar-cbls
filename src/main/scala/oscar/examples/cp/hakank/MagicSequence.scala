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
import scala.io.Source._
import scala.math._

/*

  Magic sequence problem in Oscar.
  
  http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob019/spec.html
  """
  A magic sequence of length n is a sequence of integers x0 . . xn-1 between 
  0 and n-1, such that for all i in 0 to n-1, the number i occurs exactly xi 
  times in the sequence. For instance, 6,2,1,0,0,0,1,0,0,0 is a magic sequence 
  since 0 occurs 6 times in it, 1 occurs twice, ...
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object MagicSequence extends CPModel {

  // Simple decomposition of scalarProduct
  def scalarProduct(t: Array[CPVarInt], cost: Array[Int]) = 
    sum(Array.tabulate(t.length)(i=>t(i)*cost(i)))

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var n = 10

    if (args.length > 0) {
      n = args(0).toInt
    }

    val all_values = Array.tabulate(n)(i=> (CPVarInt(cp, 0 to n-1),i))

    //
    // variables
    //
    val x = Array.fill(n)(CPVarInt(cp, 0 to n-1))

    //
    // constraints
    //
    var numSols = 0
    cp.solveAll subjectTo {

      cp.add(scalarProduct(x, Array.tabulate(n)(i=>i)) == n)
      cp.add(sum(x) == n)

      cp.add(gcc(x, all_values), Strong)
      for(i<- 0 until n) {
        cp.add(x(i) == all_values(i)._1)
      }

    } exploration {
       
      // cp.binary(x)
      // cp.binaryFirstFail(x)
      cp.binaryMaxDegree(x)

      println("\nSolution:")

      println("x: " + x.mkString(" "))

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
