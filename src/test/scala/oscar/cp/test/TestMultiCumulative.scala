package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._
import oscar.cp.scheduling.CumulativeActivity

import org.scalacheck._

class TestMultiCumulative extends FunSuite with ShouldMatchers with CPModel {
	
	test("Test : Nicolas Beldiceanu example 1") {
		
		val cp = CPSolver()
		
		val t1 = new CumulativeActivity(new CPVarInt(cp, 1 to 2), // start
										new CPVarInt(cp, 2 to 4), // duration
										new CPVarInt(cp, 3 to 6), // end
										new CPVarInt(cp, 0 to 0), // machine
										new CPVarInt(cp, -1 to 1)) // resource
		
		val t2 = new CumulativeActivity(new CPVarInt(cp, 0 to 6), // start
										new CPVarInt(cp, 0 to 2), // duration
										new CPVarInt(cp, 0 to 8), // end
										new CPVarInt(cp, 0 to 1), // machine
										new CPVarInt(cp, -3 to 4)) // resource
		
		val tasks = Array(t1, t2)
		val capacities = Array(4, 3)
		val capacities2 = Array(10, 10)
		
		val constraint1 = new MinCumulative(cp, tasks, 4, 0, false)
		val constraint2 = new MinCumulative(cp, tasks, 3, 1, false)
		
		cp.add(constraint1)
		cp.add(constraint2)
		
		t2.getEST should be(1)
		t2.getLST should be(2)
		
		t2.getECT should be(3)
		t2.getLCT should be(4)
		
		t2.getMinDuration should be(1)
		t2.getMaxDuration should be(2)
		
		t2.getMinResource should be(3)
		t2.getMaxResource should be(4)
	}
}

