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
 * Least diff problem in Oscar.
 *
 * Minimize the difference ABCDE - FGHIJ
 * where A..J are distinct digits (0..9).
 *
 * Alternative approach.
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object LeastDiff2 extends CPModel {

  // Simple decomposition of scalarProduct
  def scalarProduct(t: Array[CPVarInt], cost: Array[Int]) = 
    sum(0 until t.length)(i=>t(i)*cost(i))


   def main(args: Array[String]) {

      val cp = CPSolver()

      val n = 10
      val values = Array(10000, 1000, 100, 10, 1)

      // variables
      val all = Array.fill(n)(CPVarInt(cp, 0 to 9))
      val Array(a, b, c, d, e, f, g, h, i, j) = all


      val x = scalarProduct(Array(a,b,c,d,e), values)
      val y = scalarProduct(Array(f,g,h,i,j), values)
      val diff = x - y

      cp.minimize(diff) subjectTo {

        // constraints
	cp.add(alldifferent(all), Strong)
        cp.add(a > 0)
        cp.add(f > 0)
        cp.add(diff > 0)


      } exploration {

         cp.binaryMaxDegree(all ++ Array(diff, x, y))

         println(x + " -" +
                 y + " =" +
                 diff)
      }
	  
      cp.printStats()

  }

}
