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
	
}
