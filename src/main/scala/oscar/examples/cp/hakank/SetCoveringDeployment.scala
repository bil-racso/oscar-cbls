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

  Set covering deployment problem in Oscar.

  From http://mathworld.wolfram.com/SetCoveringDeployment.html
  """
  Set covering deployment (sometimes written 'set-covering deployment'
  and abbreviated SCDP for 'set covering deployment problem') seeks
  an optimal stationing of troops in a set of regions so that a
  relatively small number of troop units can control a large
  geographic region. ReVelle and Rosing (2000) first described
  this in a study of Emperor Constantine the Great's mobile field
  army placements to secure the Roman Empire.
  """


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SetCoveringDeployment extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

     // From http://mathworld.wolfram.com/SetCoveringDeployment.html
    val countries = Array("Alexandria",
                          "Asia Minor",
                          "Britain",
                          "Byzantium",
                          "Gaul",
                          "Iberia",
                          "Rome",
                          "Tunis")

    val n = countries.length

    // the incidence matrix (neighbours)
    val mat = Array(Array(0, 1, 0, 1, 0, 0, 1, 1),
                    Array(1, 0, 0, 1, 0, 0, 0, 0),
                    Array(0, 0, 0, 0, 1, 1, 0, 0),
                    Array(1, 1, 0, 0, 0, 0, 1, 0),
                    Array(0, 0, 1, 0, 0, 1, 1, 0),
                    Array(0, 0, 1, 0, 1, 0, 1, 1),
                    Array(1, 0, 0, 1, 1, 1, 0, 1),
                    Array(1, 0, 0, 0, 0, 1, 1, 0))
   

    //
    // variables
    //
 
    // First army
    val x = Array.fill(n)(CPVarInt(cp, 0 to 1))
    val y = Array.fill(n)(CPVarInt(cp, 0 to 1))

    // total number of elements in the choosen sets
    val num_armies = sum(x) + sum(y)


    //
    // constraints
    //
    var numSols = 0

    cp.minimize(num_armies) subjectTo {

      //
      //  Constraint 1: There is always an army in a city
      //                (+ maybe a backup)
      //                Or rather: Is there a backup, there
      //                must be an army in that city.
      //
      for(i <- 0 until n) {
        cp.add(x(i) >= y(i))
      }
      
      //
      // Constraint 2: There should always be an backup
      //               army near every city
      //
      for(i <- 0 until n) {
        cp.add(
               x(i) + 
               sum(for{j <- 0 until n if mat(i)(j) == 1} yield y(j)) 
               >= 1
               )
        
      }


    } exploration {
       
      cp.binary(x)
      // cp.binaryFirstFail(x)
      // cp.binaryMaxDegree(x)

      println("\nSolution:")

      println("num_armies: " + num_armies)
      println("x: " + x.mkString(""))
      println("y: " + y.mkString(""))

      for(i <- 0 until n) {
        var some_army = false
        if (x(i).getValue() == 1) {
          print("Army: " + countries(i) + " ")
          some_army = true
        }

        if (y(i).getValue() == 1) {
          print(" Reserve army: " + countries(i) + " ")
          some_army = true
        }

        if (some_army) {
          println()
        }
      }

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
