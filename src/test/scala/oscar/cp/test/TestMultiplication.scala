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

class TestMultiplication extends FunSuite with ShouldMatchers  {


  test("Multiplication 1") {
    val cp = CPSolver()

    val x = CPVarInt(cp,-10 to 10);
    val y = CPVarInt(cp,Set(-70,-50,50,70))
    val z = CPVarInt(cp,100 to 100);
    	
    cp.post(new oscar.cp.constraints.MulVar(x,y,z)); // should post a MulCteRes because z is fixed
    	
    var nbSol = 0
    cp.exploration {
      cp.binaryFirstFail(Array(x,y))
      ((x.isBoundTo(-2) && y.isBoundTo(-50)) || (x.isBoundTo(2) && y.isBoundTo(50))) should be(true)
      nbSol += 1
    }
    nbSol should be(2)

  }  
  

}
