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
	
	test("Test min dom not bound") {
	  val cp = CPSolver()
	  val x = Array(CPVarInt(cp, 1 to 1),CPVarInt(cp, 1 to 2),CPVarInt(cp, 1 to 3))
	  val (y: CPVarInt,i: Int) = minDomNotbound(x).head
	  i should be(1)
	}
}
