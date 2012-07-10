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
import oscar.cp.search._
import oscar.cp.modeling._

import org.scalacheck._

class TestArithmetics extends FunSuite with ShouldMatchers with CPModel {


  test("Arithmetics") {
    val cp = CPSolver()
    val i = CPVarInt(cp,1)
    val j = CPVarInt(cp,0)
    val a = CPVarInt(cp,-1 to 1)
    val b = CPVarInt(cp,0 to 1)
    val n = 8
    
    val ia = i+a
    val jb = j+b
    
        
    cp.add(ia >= 0)
    cp.add(ia <  n)
    cp.add(jb >= 0)
    cp.add(jb <  n)

    println(ia)
    println(jb)
    val ix2 = (ia)*n + (jb) // what is the index of k+1
    
    println(ix2)
    ix2.getSize() should be(18) // should contain: 0,1,8,16,17 but since we do not create holes it has 18  values

  }  
  
  

}
