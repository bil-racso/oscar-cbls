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
 ******************************************************************************/
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._

import org.scalacheck._

class TestMinimum extends FunSuite with ShouldMatchers with CPModel {


  test("Minimum1") {
    val cp = CPSolver()
    
    var x = Array.fill(3)(CPVarInt(cp, 1 to 3))
    
    var y = CPVarInt(cp, 0 to 6)
   
    cp.add(minimum(x) == y)
    
    y.getMin() should be(1)
    y.getMax() should be(3)
    
    x.foreach(w => cp.add(w >= 2))
    
    y.getMin() should be(2)


  }  
  
  test("Minimum2") {
    val cp = CPSolver()
    
    var x = Array.fill(3)(CPVarInt(cp, 1 to 3))
    
    var y = CPVarInt(cp, 0 to 6)
   
    cp.add(minimum(x) == y)
    
    cp.add(y >= 2)
    
    for (w <- x) {
      w.getMin() should be(2)
    }


  } 

}
