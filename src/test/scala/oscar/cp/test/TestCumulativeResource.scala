package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._
import oscar.cp.scheduling._
import oscar.reversible._
import org.scalacheck._

import oscar.visual._

class TestCumulativeResource extends FunSuite with ShouldMatchers {
	
	test("Test 1: packing") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 4)
		val act2 = Activity(cp, 3)
		val act3 = Activity(cp, 2)
		val acts = Array(act1, act2, act3)
		
		val resource = CumulativeResource(cp, 2)
		
		act1.needs(resource, 1)
		act2.needs(resource, 1)
		act3.needs(resource, 1)
		
		var nSol = 0
		
		val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))
		
		cp.addResourceConstraints()
		cp.solveAll 
		cp.exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		nSol should be(4)
	}
	
	test("Test 2: durations") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 3)
		val act2 = Activity(cp, 1 to 2)
		val act3 = Activity(cp, 1 to 4)
		val acts = Array(act1, act2, act3)
		
		val resource = CumulativeResource(cp, 1)
		
		act1.needs(resource, 1)
		act2.needs(resource, 1)
		act3.needs(resource, 1)
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 4), (0, 4, 3), (1, 0, 4), (1, 4, 0), (2, 0, 1), (2, 1, 0))
		
		cp.addResourceConstraints()			               
		cp.solveAll 
		cp.exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		act2.dur.value should be(1)
		act3.dur.value should be(1)
		nSol should be(6)
	}
	
	test("Test 3: height") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 4)
		val act2 = Activity(cp, 3)
		val act3 = Activity(cp, 2)
		val acts = Array(act1, act2, act3)
		
		val resource = CumulativeResource(cp, 2)
		
		act1.needs(resource, 1 to 2)
		act2.needs(resource, 1)
		act3.needs(resource, 1)
		
		var nSol = 0
		
		val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))
		
		cp.addResourceConstraints()
		cp.solveAll 
		cp.exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		resource.heightOf(act1).value should be(1)
		
		nSol should be(4)
	}
	
	test("Test 4: producer + durations") {	
		
		val horizon = 4
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 2 to 3)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2 to 3)
		val acts = Array(act1, act2, act3, act4)
		
		val resource = CumulativeResource(cp, 2)
		
		act1.needs(resource, 3)
		act2.needs(resource, 2)
		act3.needs(resource, 1)
		act4.supplies(resource, 1)
		
		
		var nSol = 0
		
		val expectedSol = Set((0, 2, 2, 0), 
							  (2, 1, 0, 1))
		
		cp.addResourceConstraints()			               
		cp.solveAll 
		cp.exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		act1.dur.value should be(2)
		act4.dur.value should be(3)
		nSol should be(2)
	}
	
	test("Test 5: producer + durations + height") {	
		
		val horizon = 4
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 2 to 3)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2 to 3)
		val acts = Array(act1, act2, act3, act4)
		
		val resource = CumulativeResource(cp, 2)
		
		act1.needs(resource, 4)
		act2.needs(resource, 3)
		act3.needs(resource, 1)
		act4.supplies(resource, 1 to 2)
		
		var nSol = 0
		
		val expectedSol = Set((0, 2, 2, 0), 
							  (2, 1, 0, 1))
		
		cp.addResourceConstraints()			               
		cp.solveAll 
		cp.exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		act1.dur.value should be(2)
		act4.dur.value should be(3)
		resource.heightOf(act4).value should be(-2)
		nSol should be(2)
	}
}