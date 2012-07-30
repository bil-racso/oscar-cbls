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

class TestOr extends FunSuite with ShouldMatchers  {
  
  
  test("or1") { 
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPVarBool(cp))
	  val B = CPVarBool(cp)
	  cp.add(new Or(A,B))
	  cp.add(A(0) == 1)
	  
	  B.isBoundTo(1) should be(true)
  }  
  
  test("or2") {
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPVarBool(cp))
	  val B = CPVarBool(cp)
	  cp.add(A(0) == 1)
	  cp.add(new Or(A,B))
	  B.isBoundTo(1) should be(true)
  } 
  
  test("or3") {
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPVarBool(cp))
	  A.foreach(x => cp.add(x == 0))
	  val B = CPVarBool(cp)
	  cp.add(new Or(A,B))
	  B.isBoundTo(0) should be(true)
  } 
  
  test("or4") {
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPVarBool(cp))
	  val B = CPVarBool(cp)
	  cp.add(new Or(A,B))
	  A.foreach(x => cp.add(x == 0))
	  B.isBoundTo(0) should be(true)
  } 
  
  test("or5") {
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPVarBool(cp))
	  val B = CPVarBool(cp)
	  cp.add(B == 1)
	  cp.add(A(0) == 0)
	  cp.add(A(1) == 0)
	  
	  cp.add(new Or(A,B))

	  A(2).isBoundTo(1) should be(true)
  }  
    
  test("or6") {
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPVarBool(cp))
	  val B = CPVarBool(cp)
	  
	  cp.add(new Or(A,B))
	  
	  cp.add(B == 1)
	  cp.add(A(0) == 0)
	  cp.add(A(1) == 0)

	  A(2).isBoundTo(1) should be(true)
  } 
  
  test("or7") {
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPVarBool(cp))
	  val B = CPVarBool(cp)
	  
	  cp.add(new Or(A,B))	  
	  cp.add(B == 0)

	  A.forall(_.isBoundTo(0)) should be(true)
  } 
  
  test("or8") {
	  val cp = CPSolver()
	  val A = Array.fill(3)(CPVarBool(cp))
	  val B = CPVarBool(cp)
	  cp.add(B == 0)
	  cp.add(new Or(A,B))	  
	  

	  A.forall(_.isBoundTo(0)) should be(true)
  }    
  
 


}
