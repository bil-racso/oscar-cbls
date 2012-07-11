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

  Bowls and Oranges problem in Oscar.

  From BitTorrent Developer Challenge
  http://www.bittorrent.com/company/about/developer_challenge
  """
  You have 40 bowls, all placed in a line at exact intervals of 
  1 meter. You also have 9 oranges. You wish to place all the oranges 
  in the bowls, no more than one orange in each bowl, so that there are 
  no three oranges A, B, and C such that the distance between A and B is 
  equal to the distance between B and C. How many ways can you arrange 
  the oranges in the bowls?.
  """
  
  Via http://surana.wordpress.com/2011/06/01/constraint-programming-example/


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object BowlsAndOranges extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 40
    val m = 9


    //
    // variables
    //
    val x = Array.fill(m)(CPVarInt(cp, 1 to n))

    //
    // constraints
    //
    var numSols = 0
    cp.solveAll subjectTo {

      // constraints
      cp.add(alldifferent(x), Strong)

      // increasing(x)
      for(i <- 1 until m) {
        cp.add(x(i-1) < x(i))
      }

      for(i <- 0 until m) {
        for(j <- 0 until i) {
          for(k <- 0 until j) {
            cp.add(x(j)-x(i) != x(k)-x(j))
          }
        }
      }
 
    } exploration {
       
      // cp.binary(x)
      // cp.binaryFirstFail(x)
      cp.binaryMaxDegree(x)

      // println(x.mkString(""))

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
