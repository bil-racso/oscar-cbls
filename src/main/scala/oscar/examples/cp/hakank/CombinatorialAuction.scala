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

  Combinatorial Auction in Oscar.
    
  This is a more general model for the combinatorial example
  in the Numberjack Tutorial, pages 9 and 24 (slides  19/175 
  and 51/175).

 
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object CombinatorialAuction extends CPModel {

  // Simple decomposition of scalarProduct
  def scalarProduct(t: Array[CPVarInt], cost: Array[Int]) = 
    sum(Array.tabulate(t.length)(i=>t(i)*cost(i)))


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 5

    // the items for each bid
    val items = Array(
                      Array(0,1),   // A,B
                      Array(0,2),   // A, C
                      Array(1,3),   // B,D
                      Array(1,2,3), // B,C,D
                      Array(0))     // A

    val bid_ids = Array(0,1,2,3)
    val bid_amount = Array(10,20,30,40,14)



    //
    // variables
    //
    val x = Array.fill(n)(CPVarInt(cp, 0 to 1))
    val z  = scalarProduct(x, bid_amount)


    //
    // constraints
    //
    var numSols = 0

    cp.maximize(z) subjectTo {

      for(bid_id <- bid_ids) {
        cp.add(sum(for{item <- 0 until n
                         i <- 0 until items(item).length
                         if items(item)(i) == bid_id} yield x(item)
            ) <= 1)

      }




    } exploration {
       
      cp.binary(x)
      // cp.binaryFirstFail(x)
      // cp.binaryMaxDegree(x)

      println("\nSolution:")

      println("z:" + z)
      println("x:" + x.mkString(""))
      println()

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
