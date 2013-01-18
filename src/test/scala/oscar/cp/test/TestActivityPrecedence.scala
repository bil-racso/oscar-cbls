package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.modeling._
import oscar.cp.scheduling._
import oscar.cp.core.Constraint


class TestActivityPrecedence extends FunSuite with ShouldMatchers {
	
	test("Test 1: end before start") {
		
		val cp = CPScheduler(7)
		
		val act1 = Activity(cp, 2)
		val act2 = Activity(cp, 2)
		val act3 = Activity(cp, 3)
		
		val resource = UnitResource(cp)
		
		act1 ends 3 beforeStartOf act2
		
		act1.start.value should be(0)
		act2.start.value should be(5)
		act3.start.size  should be(5)
		
		act1.needs(resource)
		act2.needs(resource)
		act3.needs(resource)		
		cp.addResourceConstraints()
		
		act3.start.value should be(2)
	}
	
	test("Test 2: end before end") {
		
		val cp = CPScheduler(6)
		
		val act1 = Activity(cp, 2)
		val act2 = Activity(cp, 2)
		val act3 = Activity(cp, 3)
		
		act1 ends 1 beforeStartOf act2
		act2 ends 1 beforeEndOf act3
		
		act1.start.value should be(0)
		act2.start.value should be(3)
		act3.start.value should be(3)
	}
	
	test("Test 3: start before end") {
		
		val cp = CPScheduler(10)
		
		val act1 = Activity(cp, 3)
		val act2 = Activity(cp, 3)

		act2 startsAt 0
		act1 starts 1 beforeEndOf act2
		
		val expectedSol = Set(0, 1, 2)
		var nSol = 0
		
		cp.exploration {
			cp.binary(Array(act1, act2))
			expectedSol.contains(act1.start.value) should be(true)
			nSol += 1
		} run()
		
		nSol should be(3)
	}
	
	test("Test 4: start before start") {
		
		val cp = CPScheduler(5)
		
		val act1 = Activity(cp, 3)
		val act2 = Activity(cp, 3)
		
		act2 endsAt 5
		act1 startsBeforeStartOf act2
		
		val expectedSol = Set(0, 1, 2)
		var nSol = 0
		
		cp.exploration {
			cp.binary(Array(act1, act2))
			expectedSol.contains(act1.start.value) should be(true)
			nSol += 1
		} run()
		
		nSol should be(3)
	}
	
	test("Test 5: end at end") {
		
		val cp = CPScheduler(5)
		
		val act1 = Activity(cp, 2)
		val act2 = Activity(cp, 2)

		act1 endsExactly 1 beforeEndOf act2
		
		val expectedSol = Set(0, 1, 2)
		var nSol = 0
		
		cp.exploration {
			cp.binary(Array(act1, act2))
			expectedSol.contains(act1.start.value) should be(true)
			nSol += 1
		} run()
		
		nSol should be(3)
	}
	
	test("Test 6: end at start") {
		
		val cp = CPScheduler(6)
		
		val act1 = Activity(cp, 2)
		val act2 = Activity(cp, 2)

		act1 endsAtStartOf act2
		
		val expectedSol = Set(0, 1, 2)
		var nSol = 0
		
		cp.exploration {
			cp.binary(Array(act1, act2))
			expectedSol.contains(act1.start.value) should be(true)
			nSol += 1
		} run()
		
		nSol should be(3)
	}
	
	test("Test 7: start at end") {
		
		val cp = CPScheduler(6)
		
		val act1 = Activity(cp, 2)
		val act2 = Activity(cp, 2)

		act1 startsAtEndOf act2
		
		val expectedSol = Set(2, 3, 4)
		var nSol = 0
		
		cp.exploration {
			cp.binary(Array(act1, act2))
			expectedSol.contains(act1.start.value) should be(true)
			nSol += 1
		} run()
		
		nSol should be(3)
	}
	
	test("Test 8: start at start") {
		
		val cp = CPScheduler(5)
		
		val act1 = Activity(cp, 2)
		val act2 = Activity(cp, 2)

		act1 startsExactly 1 beforeStartOf act2
		
		val expectedSol = Set(0, 1, 2)
		var nSol = 0
		
		cp.exploration {
			cp.binary(Array(act1, act2))
			expectedSol.contains(act1.start.value) should be(true)
			nSol += 1
		} run()
		
		nSol should be(3)
	}
}
