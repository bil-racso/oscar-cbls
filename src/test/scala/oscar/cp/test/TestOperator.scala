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

class TestOperator extends FunSuite with ShouldMatchers with CPModel {
  
  
  test("unary operator") { 
	  val cp = CPSolver()
	  val A = CPVarInt(cp,8)
	  val B = CPVarInt(cp,-10 to 10)
	  val C = -A
	  C.getValue() should be(-8)
	  cp.post(B == -C)
	  B.getValue() should be(8)
  }  
  
  test("boolean unary operator 1") { 
	  val cp = CPSolver()
	  val A = CPVarBool(cp,true)
	  val C = -A
	  C.getValue() should be(-1)
  }  

  test("boolean unary operator 2") { 
	  val cp = CPSolver()
	  val A = CPVarBool(cp,true)
	  val C = !A
	  C.getValue() should be(0)
  }  
  
  test("boolean unary operator 3") { 
	  val cp = CPSolver()
	  val A = CPVarBool(cp,true)
	  val C = !(!A)
	  C.isTrue() should be(true)
  }
  
  test("boolean unary operator 4") { 
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  cp.add((!A || B) == (A ==> B))
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(A,B))
	    nbSol += 1
	  }
	  nbSol should be(4)
  }
  
  test("boolean unary operator 5") { 
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  cp.add(A && B)
	  A.isTrue() should be(true)
	  B.isTrue() should be(true)
  }   

 


}
