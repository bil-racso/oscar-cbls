/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._

import org.scalacheck._

class StableMarriage extends FunSuite with ShouldMatchers  {

  test("StableMarriage") {
    val n = 5
    
    val Women = 0 until n
    val Men = 0 until n

    // for each man, what is his ranking for the women (higher is better)
    val rankWomen = Array(Array(1,2,4,3,0),
    					  Array(1,3,2,0,4),
    					  Array(4,2,1,3,0),
    					  Array(0,4,3,2,1),
    					  Array(3,2,1,0,4))

    // for each woman, what is her ranking for the men (higher is better)			
    val rankMen =   Array(Array(0,1,3,2,4),
    					  Array(2,3,0,4,1),
    					  Array(3,2,4,1,0),
    					  Array(0,4,1,3,2),
    					  Array(4,1,2,0,3))

    val cp = CPSolver()

    val wife    = Array.fill(n)(CPVarInt(cp, Women)) // wife(i) is the woman chosen for man i
    val husband = Array.fill(n)(CPVarInt(cp, Men)) // husband(j) is the man chosen for woman j


    cp.solve subjectTo {

 
      for (m <- Men) {
        cp.add(element(husband, wife(m),m))
      }
      for (w <- Women) {
        cp.add(element(wife, husband(w),w))
      }      

      for (m <- Men; w <- Women) { 
          val pref_m = element(rankMen(m),wife(m)) // preference of m for his wife
          val pref_w = element(rankWomen(w),husband(w)) // preference of w for her husband

          cp.add((pref_m >>= rankMen(m)(w)) ==> (pref_w <<= rankWomen(w)(m)))
          cp.add((pref_w >>= rankWomen(w)(m)) ==> (pref_m <<= rankMen(m)(w)))         
      }
     } exploration {
       
       cp.binary(wife)

       println()
       
       wife.map(_.getValue) should be(Array(0,2,1,4,3))
       husband.map(_.getValue) should be(Array(0,2,1,4,3))

     }
    
  }

}
