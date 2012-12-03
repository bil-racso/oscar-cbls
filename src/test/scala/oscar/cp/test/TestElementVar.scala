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

	test("Test Element Var 1") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,1 to 2),CPVarInt(cp,1 to 2),CPVarInt(cp,1 to 2))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(element(y,x,z))
		z.min should be(1)
		z.max should be(2)
	}
	
	test("Test Element Var 2") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,Set(1,3)),CPVarInt(cp,Set(4)),CPVarInt(cp,Set(1,4)))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(element(y,x,z))
		z.hasValue(2) should be(false)
		z.min should be(1)
		z.max should be(4)
		
		cp.add(y(0) != 3)
		z.hasValue(3) should be(false)
		
		cp.add(z >= 2)
		x.hasValue(0) should be(false)
		z.isBoundTo(4) should be(true)	
	}
	
	test("Test Element Var 3") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,Set(1,3)),CPVarInt(cp,Set(4)),CPVarInt(cp,Set(1,5)))
		val z = CPVarInt(cp,-20 to 100)
		
		cp.add(element(y,x,z))
		
		cp.add(x != 1)
		
		z.hasValue(4) should be(false)
		z.size should be(3) // 1,3,5
		
	}
	
	test("Test Element Var 4") {
		val cp = CPSolver()
		val x = CPVarInt(cp,-3 to 10)
		val y = Array(CPVarInt(cp,Set(1,3)),CPVarInt(cp,Set(4)),CPVarInt(cp,Set(1,5)))
		val z = CPVarInt(cp,-20 to 10000)
		
		cp.add(element(y,x,z))
		
		cp.add((y(0) !== 1) && (y(2) !== 1))
		z.min should be(3)
		z.max should be(5)
		
	}	
	

}
