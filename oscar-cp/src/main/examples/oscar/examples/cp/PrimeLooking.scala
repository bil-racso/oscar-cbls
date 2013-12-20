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
package oscar.examples.cp

import oscar.cp.modeling._

import oscar.cp.core._


/**
 * Martin Gardner Problem:
 * Call a number "prime-looking" if it is composite but not divisible by 2,3 or 5.
 * The three smallest prime-looking numbers are 49, 77 and 91. 
 * There are 168 prime numbers less than 1000. 
 * How many prime-looking numbers are there less than 1000?
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object PrimeLooking extends App {
    
    val n = 1000
    
    val cp = new CPSolver()
    val x = CPVarInt(cp,1 to n)
    val d1 = CPVarInt(cp,2 to n)
    val d2 = CPVarInt(cp,2 to n)
    
    var cpt = 0 // number of solution
    
    cp.solve subjectTo {
      cp.add(d1*d2 == x)
      cp.add(d1 <= d2) // avoid symmetric solutions
      // prevent divisibility by 2,3 and 5
      for (v <- List(2,3,5)) {
        for (i <- 1 to 1000 if (v*i <= n)) {
          cp.add(d1 != (v*i))
    	  cp.add(d2 != (v*i))
        }
      }
    } search {
      binaryFirstFail(Seq(x,d1,d2))
    } 
    
    println(cp.start())
    
  


}
