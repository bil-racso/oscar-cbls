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

   Grocery problem in Oscar.
  
   From  Christian Schulte, Gert Smolka, Finite Domain
   http://www.mozart-oz.org/documentation/fdt/
   Constraint Programming in Oz. A Tutorial. 2001.
   """
   A kid goes into a grocery store and buys four items. The cashier
   charges $7.11, the kid pays and is about to leave when the cashier
   calls the kid back, and says "Hold on, I multiplied the four items
   instead of adding them; I'll try again; Hah, with adding them the
   price still comes to $7.11''. What were the prices of the four items?
   """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
 */
object Grocery extends CPModel {

   def increasing(cp: CPSolver, y: Array[CPVarInt]) = {
     for (i <- 1 until y.length) {
       cp.add(y(i-1) <= y(i), Strong)
     }
   }


   def main(args: Array[String]) {

      val cp = CPSolver()

      val n = 4
      val m = 711
      val m2 = m*100*100*100

      println("m2: " + m2)

      // variables
      val item = Array.fill(n)(CPVarInt(cp, 1 to (m / 2).toInt))

      var numSols = 0
      cp.solveAll() subjectTo {

        cp.add(alldifferent(item), Strong)
        increasing(cp, item)       
        cp.add(sum(item) == m)

        // Note: yields overflow (though the solution is correct)
        // cp.add(item(0)*item(1)*item(2)*item(3) == m2)
        // cp.add(item(0).mul(item(1)).mul(item(2)).mul(item(3)) == m2)
        cp.add(item.reduceLeft((acc,x) => acc*x) == m2, Strong)

      } exploration {

         cp.binaryFirstFail(item)

         println(item.mkString(""))
         println()

         numSols += 1

      }
	  
      println("\nIt was " + numSols + " solutions.\n")
  
      cp.printStats()

  }

}
