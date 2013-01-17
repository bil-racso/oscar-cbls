package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._
import oscar.cp.scheduling._
import oscar.reversible._


class TestUnitResource extends FunSuite with ShouldMatchers {
	
	test("Test 1: packing") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 3)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2)
		val acts = Array(act1, act2, act3, act4)
		
		val resource1 = UnitResource(cp)
		val resource2 = UnitResource(cp)
		
		act1.needs(resource1)
		act2.needs(resource2)
		act3.needs(resource2)
		act4.needs(resource1)
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 0, 3), (0, 4, 0, 3), (0, 3, 1, 3), (0, 4, 1, 3))
		
		cp.solveAll subjectTo {
			
			act1 endsBeforeStartOf act2
			act3 endsBeforeStartOf act4
			
		} exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		nSol should be(4)
	}
	
	test("Test 2: durations") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 3 to 4)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2)
		val acts = Array(act1, act2, act3, act4)
		
		val resource1 = UnitResource(cp)
		val resource2 = UnitResource(cp)
		
		act1.needs(resource1)
		act2.needs(resource2)
		act3.needs(resource2)
		act4.needs(resource1)
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 0, 3), (0, 4, 0, 3), (0, 3, 1, 3), (0, 4, 1, 3))
		
		cp.solveAll subjectTo {
			
			act1 precedes act2
			act3 precedes act4
			
		} exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		act1.dur.value should be(3)
		nSol should be(4)
	}
	
	test("Test 3: durations") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 3 to 4) //Should be reduced to 3 
		val act2 = Activity(cp, 2)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 1)
		val acts = Array(act1, act2, act3, act4)
		
		val resource1 = UnitResource(cp)
		val resource2 = UnitResource(cp)
		
		act1 needs resource1
		act2 needs resource1
		act3 needs resource2
		act4 needs resource2
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 0, 2),
							  (0, 3, 0, 3),
							  (0, 3, 0, 4), 
							  (0, 3, 1, 0), 
							  (0, 3, 1, 3), 
							  (0, 3, 1, 4), 
							  (0, 3, 2, 0),
							  (0, 3, 2, 1), 
							  (0, 3, 2, 4),
							  (0, 3, 3, 0), 
							  (0, 3, 3, 1), 
							  (0, 3, 3, 2), 
							  (2, 0, 0, 2),
							  (2, 0, 0, 3),
							  (2, 0, 0, 4), 
							  (2, 0, 1, 0), 
							  (2, 0, 1, 3), 
							  (2, 0, 1, 4), 
							  (2, 0, 2, 0),
							  (2, 0, 2, 1), 
							  (2, 0, 2, 4),
							  (2, 0, 3, 0), 
							  (2, 0, 3, 1), 
							  (2, 0, 3, 2))
		
		cp.addResourceConstraints()
		cp.solveAll 
		cp.exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		act1.dur.value should be(3)
		nSol should be(24)
	}
}
