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

  Set covering problem in Oscar.
  
  Placing of firestations. 
  From Winston 'Operations Research', page 486.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SetCovering extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val min_distance = 15
    val num_cities = 6

    val distance = Array(Array( 0,10,20,30,30,20),
                         Array(10, 0,25,35,20,10),
                         Array(20,25, 0,15,30,20),
                         Array(30,35,15, 0,15,25),
                         Array(30,20,30,15, 0,14),
                         Array(20,10,20,25,14, 0))

    //
    // variables
    //
 
    val x = Array.fill(num_cities)(CPVarInt(cp, 0 to 1))
    val z = sum(x)


    //
    // constraints
    //
    var numSols = 0

    cp.minimize(z) subjectTo {

      // ensure that all cities are covered
      for(i <- 0 until num_cities) {
        cp.add(
               sum(
                   for{
                     j <- 0 until num_cities
                     if distance(i)(j) <= min_distance
                   } yield x(j)
                   ) >= 1
               )
      }
      
    } exploration {
       
      cp.binary(x)
      // cp.binaryFirstFail(x)
      // cp.binaryMaxDegree(x)


      println("\nSolution:")

      println("x: " + x.mkString(""))
      println("z: " + z)

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
