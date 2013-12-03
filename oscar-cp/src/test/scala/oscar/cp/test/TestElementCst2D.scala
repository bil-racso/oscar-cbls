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
import oscar.cp.constraints.ElementCst2D

/**
 * Test on Element Constraint on a 2D array
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestElementCst2D extends FunSuite with ShouldMatchers {

	test("Test Element 1") {
		val cp = CPSolver()
		val T = Array(Array(1,3,2),
		              Array(6,8,1))
		val x = CPVarInt(cp,-3 to 10)
		val y = CPVarInt(cp,-4 to 14)
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(new ElementCst2D(T,x,y,z))
			
		x.min should be(0)
		x.max should be(1)
		y.min should be(0)
		y.max should be(2)
		z.min should be(1)
		z.max should be(8)
	}
	
	test("Test Element 2") {
		val cp = CPSolver()
		val T = Array(Array(1,3,2),
		              Array(6,8,1))
		val x = CPVarInt(cp,-3 to 10)
		val y = CPVarInt(cp,-4 to 14)
		val z = CPVarInt(cp,7 to 8)
		
		cp.add(new ElementCst2D(T,x,y,z))
			
		x.min should be(1)
		x.max should be(1)
		y.min should be(1)
		y.max should be(1)
		z.min should be(8)
		z.max should be(8)
	}
	
	test("Test Element 3") {
		val cp = CPSolver()
		val T = Array(Array(1,7,2),
		              Array(6,8,1))
		val x = CPVarInt(cp,-3 to 10)
		val y = CPVarInt(cp,-4 to 14)
		val z = CPVarInt(cp,7 to 8)
		
		cp.add(new ElementCst2D(T,x,y,z))
			
		x.min should be(0)
		x.max should be(1)
		y.min should be(1)
		y.max should be(1)
		z.min should be(7)
		z.max should be(8)	
	}
	
	test("Test Element 4") {
		val cp = CPSolver()
		val T = Array(Array(1,7,2),
		              Array(6,8,1))
		val x = CPVarInt(cp,-3 to 10)
		val y = CPVarInt(cp,-4 to 14)
		val z = CPVarInt(cp,9 to 10)
		
		cp.post(new ElementCst2D(T,x,y,z))
			
		cp.isFailed() should be(true)
	}
	
	test("Test Element 5") {
		val cp = CPSolver()
		val T = Array(Array(1,3,2),
		              Array(6,8,1))
		val x = CPVarInt(cp,-3 to 10)
		val y = CPVarInt(cp,-4 to 14)
		val z = CPVarInt(cp,0 to 10)
		
		cp.add(new ElementCst2D(T,x,y,z))
		cp.add(z >= 4)
		
		x.min should be(1)
		y.max should be(1)
		z.min should be(6)
		z.max should be(8)
	}
	
	test("Test Element 6") {
		val cp = CPSolver()
		val T = Array(Array(1,3,2),
		              Array(1,8,4))
		val x = CPVarInt(cp,-3 to 10)
		val y = CPVarInt(cp,-4 to 14)
		val z = CPVarInt(cp,0 to 10)
		
		cp.add(new ElementCst2D(T,x,y,z))
		cp.add(y != 0)
		
		z.min should be(2)
		z.max should be(8)
	}	
	
	test("Test Element 7") {
		val cp = CPSolver()
		val T = Array(Array(1,3,2),
		              Array(1,8,4))
		val x = CPVarInt(cp,-3 to 10)
		val y = CPVarInt(cp,-4 to 14)
		val z = CPVarInt(cp,0 to 10)
		
		cp.add(new ElementCst2D(T,x,y,z))
		cp.add(y != 0)
		cp.add(z < 4)
		
		x.max should be(0)
		z.min should be(2)
		z.max should be(3)
	}	
	

}
