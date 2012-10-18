package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.core._
import oscar.cp.modeling._

class TestOpposite extends FunSuite with ShouldMatchers {

	test("Test Opposite 1") {
		val cp = CPSolver()
		val a = CPVarInt(cp, Set(0, 1, 2, 3, 4))
		val b = -a
		b.max should be(0)
		b.min should be(-4)
		cp.add(a >= 1)		
		b.max should be(-1)
		cp.add(a <= 3)
		b.min should be(-3)
	}
	

}
