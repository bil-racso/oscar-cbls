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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._

class TestImplication extends FunSuite with ShouldMatchers  {
  
  
  test("=>1") {
      
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  cp.add(res == 0)
	  A.isBoundTo(1) should be(true)
	  B.isBoundTo(0) should be(true)
  }  
  
  test("=>2") {
      val values = Set((0,0,1),(0,1,1),(1,0,0),(1,1,1))
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(A,B))
	    val entry = (A.getValue,B.getValue,res.getValue)
	    values.contains(entry) should be(true)
	    nbSol += 1
	  } run()
	  nbSol should be(4)
  }
  
  test("=>3") {
      val values = Set((0,0,1),(0,1,1),(1,0,0),(1,1,1))
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(res,A,B))
	    val entry = (A.getValue,B.getValue,res.getValue)
	    values.contains(entry) should be(true)
	    nbSol += 1
	  } run()
	  nbSol should be(4)
  } 
  
    test("=>4") {
      val values = Set((0,0,1),(0,1,1),(1,0,0),(1,1,1))
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(res,B,A))
	    val entry = (A.getValue,B.getValue,res.getValue)
	    values.contains(entry) should be(true)
	    nbSol += 1
	  } run()
	  nbSol should be(4)
  } 
    
  test("=>5") {
      val values = Set((0,0,1),(0,1,1),(1,0,0),(1,1,1))
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  val res = A ==> B
	  var nbSol = 0
	  cp.exploration {
	    cp.binary(Array(B,A))
	    val entry = (A.getValue,B.getValue,res.getValue)
	    values.contains(entry) should be(true)
	    nbSol += 1
	  } run()
	  nbSol should be(4)
  }
  
  test("=>6") {
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  cp.add(A ==> B)
	  cp.add(B == 0)
	  A.isBoundTo(0) should be(true)
  }
  
  test("=>7") {
	  val cp = CPSolver()
	  val A = CPVarBool(cp)
	  val B = CPVarBool(cp)
	  cp.add(A == 1)
	  cp.add(A ==> B)
	  B.isBoundTo(1) should be(true)
  }

    
    
  
 
  


}
