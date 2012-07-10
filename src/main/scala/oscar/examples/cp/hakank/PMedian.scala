/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.example.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

  P-median problem in Oscar.

  Model and data from the OPL Manual, which describes the problem:
  """
  The P-Median problem is a well known problem in Operations Research.
  The problem can be stated very simply, like this: given a set of customers
  with known amounts of demand, a set of candidate locations for warehouses,
  and the distance between each pair of customer-warehouse, choose P
  warehouses to open that minimize the demand-weighted distance of serving
  all customers from those P warehouses.
  """


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object PMedian extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val p = 2
    val num_customers = 4

    val CUSTOMERS = 0 until num_customers

    val num_warehouses = 3;
    val WAREHOUSES = 0 until num_warehouses

    val demand = Array(100,80,80,70)
    val distance = Array(
                         Array( 2, 10, 50),
                         Array( 2, 10, 52),
                         Array(50, 60,  3),
                         Array(40, 60,  1))



    //
    // variables
    //
    val open = Array.fill(num_warehouses)(CPVarInt(cp, 0 to num_warehouses))
    val ship = Array.fill(num_customers)(Array.fill(num_warehouses)(CPVarInt(cp, 0 to 1)))

    val z = CPVarInt(cp, 0 to 1000)
      
    //
    // constraints
    //
    var numSols = 0

    cp.minimize(z) subjectTo {

      cp.add(sum(open) == p)

      for(c <- CUSTOMERS) {
        for(w <- WAREHOUSES) {
          cp.add(ship(c)(w) <= open(w))
        }
        cp.add(sum(for(w <- WAREHOUSES) yield ship(c)(w)) == 1)
      }
        
      cp.add(z == sum(
                for{c <- CUSTOMERS
                    w <- WAREHOUSES} yield ship(c)(w)*demand(c)*distance(c)(w)
                      ))
    

    } exploration {
       
      // cp.binary(ship.flatten ++ open)
      // cp.binaryFirstFail(ship.flatten ++ open)
      cp.binaryMaxDegree(ship.flatten ++ open)

      println("z: " + z)
      println("open: " + open.mkString(""))
      println("ship:")
      for(c <- CUSTOMERS) {
        println(ship(c).mkString(""))
      }
      println()

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
