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


/**
 *
 * Map coloring in Oscar
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object MapColoring extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val num_colors = 4
    val num_countries = 6

    // Belgium, Denmark, France, Germany, Netherlands, Luxembourg
    val connections = List(List(0, 0, 1, 1, 1, 1),
                           List(0, 0, 0, 1, 0, 0),
                           List(1, 0, 0, 1, 1, 0),
                           List(1, 1, 1, 0, 1, 1),
                           List(1, 0, 1, 1, 0, 0),
                           List(1, 0, 0, 1, 0, 0))
      
    // variables
    val color = Array.fill(num_countries)(CPVarInt(cp, 1 to num_colors))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      for (c1 <- 0 until num_countries; c2 <- 0 until c1) {
        if (connections(c1)(c2)==1) {
          cp.add(color(c1) != color(c2))
        }
      }

      // Symmetry breaking: Belgium has color 1
      cp.add(color(0) == 1)


     } exploration {
       
       cp.binaryFirstFail(color)

       println("color:" + color.mkString(" "))

       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
