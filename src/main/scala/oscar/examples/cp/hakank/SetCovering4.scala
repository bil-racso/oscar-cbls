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

  Set covering and set partition problem in Oscar.

  Example from the Swedish book
  Lundgren, Roennqvist, Vaebrand
  'Optimeringslaera' (translation: 'Optimization theory'),
  page 408.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SetCovering4 extends CPModel {

  // Simple decomposition of scalarProduct
  def scalarProduct(t: Array[CPVarInt], cost: Array[Int]) = 
    sum(Array.tabulate(t.length)(i=>t(i)*cost(i)))


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val num_alternatives = 10
    val num_objects = 8
    
    // select set partition / set covering
    val set_partition = 1
    // val set_partition = 0

    // costs for the alternatives
    val costs = Array(19, 16, 18, 13, 15, 19, 15, 17, 16, 15)

    // the alternatives, and their objects
    val a = Array(
                     // 1 2 3 4 5 6 7 8    the objects
                  Array(1,0,0,0,0,1,0,0),  // alternative 1
                  Array(0,1,0,0,0,1,0,1),  // alternative 2
                  Array(1,0,0,1,0,0,1,0),  // alternative 3
                  Array(0,1,1,0,1,0,0,0),  // alternative 4
                  Array(0,1,0,0,1,0,0,0),  // alternative 5
                  Array(0,1,1,0,0,0,0,0),  // alternative 6
                  Array(0,1,1,1,0,0,0,0),  // alternative 7
                  Array(0,0,0,1,1,0,0,1),  // alternative 8
                  Array(0,0,1,0,0,1,0,1),  // alternative 9
                  Array(1,0,0,0,0,1,1,0))  // alternative 10

    //
    // variables
    //
 
    val x = Array.fill(num_alternatives)(CPVarInt(cp, 0 to 1))
    val z = scalarProduct(x, costs)


    //
    // constraints
    //
    var numSols = 0

    cp.minimize(z) subjectTo {

      for(j <- 0 until num_objects) {       
        val b = sum(for{i <- 0 until num_alternatives} yield x(i) * a(i)(j))

        if (set_partition == 1) {
          cp.add(b >= 1)
        } else {
          cp.add(b == 1)
        }
      }


    } exploration {
       
      cp.binary(x)
      // cp.binaryFirstFail(x)
      // cp.binaryMaxDegree(x)

      println("\nSolution:")

      println("z: " + z)
      println("x: " + x.mkString(""))
      println("Selected alternatives: " + 
              x.zipWithIndex.filter(_._1.getValue() == 1).map(_._2).mkString(" "))

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
