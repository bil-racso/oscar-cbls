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

import oscar.cp.core._
import oscar.cp.modeling._

class TestCPVarInt extends FunSuite with ShouldMatchers {

	test("Test1 : Median") {

		val cp = CPSolver()
		
		val a = CPVarInt(cp, Array(0, 1, 2, 3, 4))
		
		a.median should be(2)
		
		a.removeValue(1)
		
		a.median should be(3)
		
		a.removeValue(4)
		
		a.median should be(2)
	}
	

	
	test("Test is full") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(0,1,3,2))
	  x.isFull should be(true)
	  cp.add(x!=3)
	  x.isFull should be(true)
	  cp.add(x!=1)
	  x.isFull should be(false)
	  val y = CPVarInt(cp,Set(0,1,3))
	  x.isFull should be(false)
	  
	}
	
	
	test("Iterator1") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(0,1,3,2))
	  x.toSet should be(Set(0,1,2,3))
	}	
	
	test("Iterator2") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(0,1,3,2))+1
	  x.toSet should be(Set(1,2,3,4))
	}	
	
	test("Iterator3") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(1,3))-1
	  x.toSet should be(Set(0,2))
	}
	
	test("Iterator4") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,Set(1,3,5))-1
	  cp.add(x != 2)
	  x.toSet should be(Set(0,4))
	}
	
	test("Iterator5") {
	  val cp = CPSolver()
	  val x = CPVarInt(cp,1 to 5) -1
	  
	  x.toSet should be(Set(0,1,2,3,4))
	  cp.add(x != 2)
	  x.toSet should be(Set(0,1,3,4))
	}
	
	test("isBound test - var does get bound") {
		val cp = CPSolver()
		val a = CPVarInt(cp, Array(10, 20, 30))
		a.isBound should be(false)
		cp.add(a == 10)
		a.isBound should be(true)
	}
	
	test("isBound test - var doesn't get bound") {
		val cp = CPSolver()
		val a = CPVarInt(cp, Array(10, 20, 30))
		a.isBound should be(false)
		
		evaluating {
			cp.add(a < 10)
		} should produce [NoSolutionException]
	}
}
