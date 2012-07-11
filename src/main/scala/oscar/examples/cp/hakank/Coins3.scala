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

  Coins puzzle in Oscar.
  
  
  From "Constraint Logic Programming using ECLiPSe"
  pages 99f and 234 ff.
  The solution in ECLiPSe is at page 236.
  """
  What is the minimum number of coins that allows one to pay _exactly_
  any amount smaller than one Euro? Recall that there are six different
  euro cents, of denomination 1, 2, 5, 10, 20, 50
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Coins3 extends CPModel {

  // Simple decomposition of scalarProduct
  def scalarProduct(t: Array[CPVarInt], cost: Array[Int]) = 
    sum(Array.tabulate(t.length)(i=>t(i)*cost(i)))


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 6  // number of different coins
    val variables = Array(1,2,5,10,25,50)


    //
    // variables
    //
    val x = Array.fill(n)(CPVarInt(cp, 0 to 99))
    val num_coins  = sum(x)


    //
    // constraints
    //
    var numSols = 0

    cp.minimize(num_coins) subjectTo {

      // Check that all changes from 1 to 99 can be made.
      for(j <- 1 until 100) {
        val tmp = Array.fill(n)(CPVarInt(cp, 0 to 99))
        cp.add(scalarProduct(tmp, variables) == j)
        
        for(i <- 0 until n) {
          cp.add(tmp(i) <= x(i))
        }
        
      }

    } exploration {
       
      cp.binary(x)
      // cp.binaryFirstFail(x)
      // cp.binaryMaxDegree(x)

      println("\nSolution:")

      println("num_coins : " + num_coins)
      println("x: " + x.mkString(""))
      println()

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
