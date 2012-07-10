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
 * Problem from Martin Gardner (February 1967):
 * """
 * The integers 1,3,8, and 120 form a set with a remarkable property: the
 * product of any two integers is one less than a perfect square. Find
 * a fifth number that can be added to the set without destroying
 * this property.
 * """
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object CuriousSetOfIntegers extends CPModel {


   def increasing(cp: CPSolver, y: Array[CPVarInt]) = {
     for (i <- 1 until y.length) {
       cp.add(y(i-1) <= y(i))
     }
   }


   def main(args: Array[String]) {

      val cp = CPSolver()

      //
      // data
      // 
      val n = 5
      val max_val = 10000

      //
      // decision variables
      // 
      val x = Array.fill(n)(CPVarInt(cp, 0 to max_val))

      var numSols = 0
      cp.solveAll() subjectTo {

	cp.add(alldifferent(x), Strong)

        for(i <- 0 until n - 1) {
          for(j <- i + 1 until n) {
            val p = CPVarInt(cp, 0 to max_val)
            cp.add((p*p- 1) == x(i) * x(j))
          }
        }

        // Symmetry breaking
        increasing(cp, x)

        // This is the original problem:
        // The given numbers are {1,3,8,120},
        // Which is the fifth number?
        cp.add(
               ( (x(0) <<= 1) && x(1) === 1 && x(2) === 3 && x(3) === 8 && x(4) === 120)
               ||
               (x(0) === 1 && x(1) === 3 && x(2) === 8 && x(3) === 120 && (x(4) >>= 120))
               )

      } exploration {

        cp.binary(x)
        println(x.mkString(""))

        numSols += 1
      }

      println("\nIt was " + numSols + " solutions.")	  
      cp.printStats()

  }

}
