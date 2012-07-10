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

  Bus scheduling in Oscar.
  
  Minimize number of buses in timeslots.

  Problem from Taha "Introduction to Operations Research", page 58.
   
  Note: This is a slightly more general model than Taha's.
 

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object BusSchedule extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val time_slots = 6
    // min number of buses for each time slot
    val demands = Array(8, 10, 7, 12, 4, 4)
    val max_num = demands.sum


    //
    // variables
    //
 
    // How many buses start the schedule at time slot t
    val x = Array.fill(time_slots)(CPVarInt(cp, 0 to max_num))
    // Total number of buses
    val num_buses  = sum(x)



    //
    // constraints
    //
    var numSols = 0

    cp.minimize(num_buses) subjectTo {

      // Meet the demands for this and the next time slot.
      for(i <- 0 until time_slots - 1) {
        cp.add(x(i)+x(i+1) >= demands(i))
      }

      // The demand "around the clock"
      cp.add(x(time_slots-1) + x(0) - demands(time_slots-1) == 0)
      
      
    } exploration {
       
      cp.binary(x)
      // cp.binaryFirstFail(x)
      // cp.binaryMaxDegree(x)

      println("\nSolution:")

      println("x: " + x.mkString(""))
      println("num_buses : " + num_buses)
      println()

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
