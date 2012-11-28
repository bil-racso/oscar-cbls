/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.core._
import oscar.cp.modeling._
import oscar.cp.constraints.ElementCst2D

/**
 * Test on Element Var Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestElementVar extends FunSuite with ShouldMatchers {

  	// --------- gac element var ----------
  
	test("Test Element Var AC 1") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,1 to 2),CPVarInt(cp,1 to 2),CPVarInt(cp,1 to 2))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(elementVar(y,x,z),Strong)
		z.min should be(1)
		z.max should be(2)
	}
	
	test("Test Element Var AC 2") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,Set(1,3)),CPVarInt(cp,Set(4)),CPVarInt(cp,Set(1,4)))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(elementVar(y,x,z),Strong)
		z.hasValue(2) should be(false)
		z.min should be(1)
		z.max should be(4)
		
		cp.add(y(0) != 3)
		z.hasValue(3) should be(false)
		
		cp.add(z >= 2)
		x.hasValue(0) should be(false)
		z.isBoundTo(4) should be(true)	
	}
	
	test("Test Element Var AC 3") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,Set(1,3)),CPVarInt(cp,Set(4)),CPVarInt(cp,Set(1,5)))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(elementVar(y,x,z),Strong)
		
		cp.add(x != 1)
		
		z.hasValue(4) should be(false)
		z.size should be(3) // 1,3,5
		
	}
	
	test("Test Element Var AC 4") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,Set(1,3)),CPVarInt(cp,Set(4)),CPVarInt(cp,Set(1,5)))
		val z = CPVarInt(cp,-20 to 10000)
		
		cp.add(elementVar(y,x,z),Strong)
		
		cp.add((y(0) !== 1) && (y(2) !== 1))
		z.min should be(3)
		z.max should be(5)
		
	}
	
	test("Test Element Var AC 5") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,1 to 3),CPVarInt(cp,2 to 2),CPVarInt(cp,2 to 2))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(elementVar(y,x,z),Strong)
		
		z.min should be(1)
		cp.add(z < 2)
		x.isBound should be(true)
		x.value should be(0)
		y(0).isBound should be(true)
		y(0).value should be(1)
	}	
	
	// --------- bound consistent elementVar var ----------
	
	test("Test Element Var BC 1") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,1 to 2),CPVarInt(cp,1 to 2),CPVarInt(cp,1 to 2))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(elementVar(y,x,z),Weak)
		z.min should be(1)
		z.max should be(2)
		x.min should be(0)
		x.max should be(2)
	}	

	test("Test Element Var BC 2") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,1 to 3),CPVarInt(cp,2 to 2),CPVarInt(cp,2 to 2))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(elementVar(y,x,z),Weak)
		
		z.min should be(1)
		cp.add(x != 0)
		z.min should be(2)
	}
	
	test("Test Element Var BC 3") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,1 to 3),CPVarInt(cp,2 to 2),CPVarInt(cp,2 to 2))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(elementVar(y,x,z),Weak)
		
		z.min should be(1)
		cp.add(z < 2)
		
		x.isBound should be(true)
		x.value should be(0)
		y(0).isBound should be(true)
		y(0).value should be(1)
		
	}		

}
