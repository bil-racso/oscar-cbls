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

import oscar.cp._

/**
 *
 * SEND+MORE MONEY problem in Oscar.
 * 
 * A slightly different approach.
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object SendMoreMoney2 {

   def main(args: Array[String]) {

      val cp = CPSolver()

      // variables
      val all = Array.fill(8)(CPIntVar(0 to 9)(cp))
      val Array(s,e,n,d,m,o,r,y) = all

      cp.solve subjectTo {

        // constraints
        cp.add(       s*1000 + e*100 + n*10 + d +
                      m*1000 + o*100 + r*10 + e ==
            m*10000 + o*1000 + n*100 + e*10 + y)
        cp.add(s > 0)
        cp.add(m > 0)
        cp.add(allDifferent(all), Strong)

      } search {
         binaryFirstFail(all)
      } onSolution {
         println(all.mkString(""))
      } 
      println(cp.start())
      
  }

}
