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

class TestStretchWrapper extends FunSuite with ShouldMatchers with CPModel {


  test("test stretch 1") {
    val cp = CPSolver()
    var x = Array.fill(6)(CPVarInt(cp,0 to 2))
    val automaton = stretchAutomaton(x, 2, 2)
    cp.add(regular(x,automaton))
    cp.add(x(5) == 0)
    cp.getStatus() should not be === (CPOutcome.Failure)
    
    //  0-2  0-2  1-2  1-2  0   0
    println(x.mkString(","))
    x(4).getValue() should be(0)
    x(3).hasValue(0) should be(false)
    x(2).hasValue(0) should be(false)
      
  }
  
 
  

  

}
