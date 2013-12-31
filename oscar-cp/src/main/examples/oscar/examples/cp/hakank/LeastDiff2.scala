/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._

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
object LeastDiff2 {

  def main(args: Array[String]) {

    val cp = CPSolver()

    val n = 10
    val values = Array(10000, 1000, 100, 10, 1)

    // variables
    val all = Array.fill(n)(CPVarInt(0 to 9)(cp))
    val Array(a, b, c, d, e, f, g, h, i, j) = all

    val x = weightedSum(values, Array(a, b, c, d, e))
    val y = weightedSum(values, Array(f, g, h, i, j))
    val diff = x - y

    cp.minimize(diff) subjectTo {

      // constraints
      cp.add(allDifferent(all), Strong)
      cp.add(a > 0)
      cp.add(f > 0)
      cp.add(diff > 0)

    } search {

      binaryMaxDegree(all ++ Array(diff, x, y))
    
    } onSolution {
      
      println(x + " -" +
        y + " =" +
        diff)
        
    }

    println(cp.start())

  }

}
