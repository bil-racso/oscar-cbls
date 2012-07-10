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
import scala.math._


/**
 *
 * ToNum in Oscar.
 *
 * Channelling between an array of variables and a variable (number).
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object ToNum extends CPModel {

  // channeling between IntVar array t <=> IntVar s
  def toNum(t: Array[CPVarInt], base: Int=10) = sum(
      Array.tabulate(t.length)(i=> t(i)*pow(base, t.length-i-1).toInt))

   def main(args: Array[String]) {

      val cp = CPSolver()

      val n = 4
      val base = 10

      // variables
      val x = Array.tabulate(n)(i => CPVarInt(cp, 0 to base-1))
      val y = CPVarInt(cp, 0 to pow(n, base).toInt)

      var numSols = 0
      cp.solveAll subjectTo {

        cp.add(y == toNum(x))
        // cp.add(y == 2143)

      } exploration {

         cp.binaryFirstFail(x)

         println("x:" + x.mkString("") + "  y:" + y)

         numSols += 1     

      }

      println("\nIt was " + numSols + " solutions.")
	  
      cp.printStats()

  }

}
