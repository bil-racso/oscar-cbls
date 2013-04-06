package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.core._
import oscar.cp.modeling._
import oscar.cp.constraints.Permutation

class TestPermutation extends FunSuite with ShouldMatchers {

	test("Test Permutation 1") {
		val cp = CPSolver()
		val x = Array.fill(4)(CPVarInt(cp,-6 to 6))
		val y = Array.fill(4)(CPVarInt(cp,-6 to 6))
		
		cp.add(x(0) >= 2)
		cp.add(new Permutation(x,y))

		
		y(0).hasValue(0) should be(false)
		y(1).hasValue(0) should be(false)
		y(2).hasValue(0) should be(true)
		
	}
	
	test("Test Permutation 2") {
		val cp = CPSolver()
		val x = Array.fill(4)(CPVarInt(cp,-6 to 6))
		val y = Array.fill(4)(CPVarInt(cp,-6 to 6))

		cp.add(new Permutation(x,y))

		x.forall(_.min == 0) should be(true)
		y.forall(_.min == 0) should be(true)
		x.forall(_.max == 3) should be(true)
		y.forall(_.max == 3) should be(true)
		
		cp.add(x(0) >= 2)
		
		
		y(0).hasValue(0) should be(false)
		y(1).hasValue(0) should be(false)
		y(2).hasValue(0) should be(true)
		
	}
	
	test("Test Permutation 4") {
		val cp = CPSolver()
		val x = Array.fill(4)(CPVarInt(cp,-6 to 6))
		val y = Array.fill(4)(CPVarInt(cp,-6 to 6))

		cp.add(new Permutation(x,y))
		cp.add(x(2) == 2)
		
		
		y(2).isBound should be(true)
		y(2).value should be(2)
		y(3).hasValue(2) should be(false)
		
		cp.add(x(0) == 3)
	    y(3).isBound should be(true)
		y(3).value should be(0)
		
	}		
	
	test("Test Permutation 5") {
		val cp = CPSolver()
		val x = Array.fill(4)(CPVarInt(cp,-6 to 6))
		val y = Array.fill(4)(CPVarInt(cp,-6 to 6))

		cp.add(new Permutation(x,y))
		cp.add(y(2) == 2)
		
		
		x(2).isBound should be(true)
		x(2).value should be(2)
		x(3).hasValue(2) should be(false)
		
	}	

}
