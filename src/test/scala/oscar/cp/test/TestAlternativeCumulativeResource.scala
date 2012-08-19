package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._
import oscar.cp.scheduling._

import org.scalacheck._

class TestAlternativeCumulativeResource extends FunSuite with ShouldMatchers {
	
	test("Test 1: packing") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 4)
		val act2 = Activity(cp, 3)
		val act3 = Activity(cp, 2)
		
		val resourceSet = AlternativeCumulativeResource(cp, 2, 2)
		
		act1 needs 1 ofResources (0 to 1) in resourceSet
		act2 needs 1 ofResources (0 to 1) in resourceSet
		act3 needs 1 ofResources (0 to 1) in resourceSet
		
		var nSol = 0
		val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))
		
		cp.solveAll subjectTo {
			
			cp.add(resourceSet.resourcesOf(act1) == 0)
			cp.add(resourceSet.resourcesOf(act2) == 0)
			cp.add(resourceSet.resourcesOf(act3) == 0)	
			
		} exploration {
			
			cp.binary(cp.activities)
			
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
		
		val resourceSet = AlternativeCumulativeResource(cp, 2, 1)
		
		act1 needs 1 ofResources (0 to 1) in resourceSet
		act2 needs 1 ofResources (0 to 1) in resourceSet
		act3 needs 1 ofResources (0 to 1) in resourceSet
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 4), (0, 4, 3), (1, 0, 4), (1, 4, 0), (2, 0, 1), (2, 1, 0))
				               
		cp.solveAll subjectTo {
			
			cp.add(resourceSet.resourcesOf(act1) == 0)
			cp.add(resourceSet.resourcesOf(act2) == 0)
			cp.add(resourceSet.resourcesOf(act3) == 0)	
			
		} exploration {
			
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
		
		val resourceSet = AlternativeCumulativeResource(cp, 2, 2)
		
		act1 needs (1 to 2) ofResources (0 to 1) in resourceSet
		act2 needs 1 ofResources (0 to 1) in resourceSet
		act3 needs 1 ofResources (0 to 1) in resourceSet
		
		var nSol = 0
		
		val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))
		
		cp.solveAll subjectTo {
			
			cp.add(resourceSet.resourcesOf(act1) == 0)
			cp.add(resourceSet.resourcesOf(act2) == 0)
			cp.add(resourceSet.resourcesOf(act3) == 0)	
			
		} exploration {
			
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		resourceSet.heightOf(act1).value should be(1)
		
		nSol should be(4)
	}
	
	test("Test 4: alternatives") {	
		
		val horizon = 4
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 2 to 3)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2 to 3)
		val acts = Array(act1, act2, act3, act4)
		
		val resourceSet = AlternativeCumulativeResource(cp, 2, 2)
		
		act1 needs 3 ofResources (0 to 1) in resourceSet
		act2 needs 2 ofResources (0 to 1) in resourceSet
		act3 needs 1 ofResources (0 to 1) in resourceSet
		act4 gives 1 toResources (0 to 1) in resourceSet
		
		
		var nSol = 0
		val expectedSol = Set((0, 2, 2, 0), 
							  (2, 1, 0, 1))
		
		cp.solveAll subjectTo {
			
			cp.add(resourceSet.resourcesOf(act1) == 0)
			cp.add(resourceSet.resourcesOf(act2) == 0)
			cp.add(resourceSet.resourcesOf(act3) == 0)	
			
		} 
		
		resourceSet.resourcesOf(act4).value should be(0)
		
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
	
	test("Test 5: alternatives") {	
		
		val horizon = 4
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 2 to 3)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2 to 3)
		val acts = Array(act1, act2, act3, act4)
		
		val resourceSet = AlternativeCumulativeResource(cp, 2, 2)
		
		act1 needs 4 ofResources (0 to 1) in resourceSet
		act2 needs 3 ofResources (0 to 1) in resourceSet
		act3 needs 1 ofResources (0 to 1) in resourceSet
		act4 gives (1 to 2) toResources (0 to 1) in resourceSet
		
		var nSol = 0
		
		val expectedSol = Set((0, 2, 2, 0), 
							  (2, 1, 0, 1))
		
		cp.solveAll subjectTo {
			
			cp.add(resourceSet.resourcesOf(act1) == 0)
			cp.add(resourceSet.resourcesOf(act2) == 0)
			cp.add(resourceSet.resourcesOf(act3) == 0)	
			
		} 
		
		resourceSet.resourcesOf(act4).value should be(0)
		
		cp.exploration {
			
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		}
		
		act1.dur.value should be(2)
		act4.dur.value should be(3)
		resourceSet.heightOf(act4).value should be(-2)
		nSol should be(2)
	}
	
	test("Test 6: alternatives") {	
		
		val horizon = 6
		val cp = new CPScheduler(horizon)		
		val req = Array(1,4,3,2)
		
		val act1 = Activity(cp, 6)
		val act2 = Activity(cp, 6)
		val act3 = Activity(cp, 6)
		val act4 = Activity(cp, 6)
		val acts = Array(act1, act2, act3, act4)
		
		val resourceSet = AlternativeCumulativeResource(cp, 2, 5)
		
		act1 needs 1 ofResources (0 to 1) in resourceSet
		act2 needs 4 ofResources (0 to 1) in resourceSet
		act3 needs 3 ofResources (0 to 1) in resourceSet
		act4 needs 2 ofResources (0 to 1) in resourceSet
		
		var nbSol = 0

		cp.solveAll 
		cp.subjectTo {
			
			cp.add(resourceSet.resourcesOf(act2) == 0)
		} 
		
		resourceSet.resourcesOf(act2).value should be(0)
		resourceSet.resourcesOf(act3).value should be(1)
		resourceSet.resourcesOf(act4).value should be(1)
		
		cp.exploration {
			
			cp.binary(acts.map(resourceSet.resourcesOf(_)))
			nbSol += 1
		}	
		
		nbSol should be(1)
	}
	
	test("Test 7: alternatives") {	
		
		val horizon = 106
		val cp = new CPScheduler(horizon)	
		
		val act1 = Activity(cp, 6)
		val act2 = Activity(cp, 6)
		val act3 = Activity(cp, 6)
		val act4 = Activity(cp, 6)
		val act5 = Activity(cp, 6)
		val acts = Array(act1, act2, act3, act4, act5)
		
		val resource1 = MaxResource(cp, 5)
		val resource2 = MaxResource(cp, 5)
		
		val resourceSet = AlternativeCumulativeResource(resource1, resource2)
		
		act1 needs 1 ofResources (0 to 1) in resourceSet
		act2 needs 4 ofResources (0 to 1) in resourceSet
		act3 needs 3 ofResources (0 to 1) in resourceSet
		act4 needs 2 ofResources (0 to 1) in resourceSet
		act5 needs 1 ofResources (0 to 1) in resourceSet
		
		cp.add(act1.start == 0)
		cp.add(act2.start == 0)
		cp.add(act3.start == 0)
		cp.add(act4.start == 0)  
		
		cp.add(resourceSet.resourcesOf(act2) == 0)
		
		cp.addResourceConstraints()
		
		resourceSet.resourcesOf(act1).value should be(0)
		resourceSet.resourcesOf(act3).value should be(1)
		resourceSet.resourcesOf(act4).value should be(1)
		
		resourceSet.resourcesOf(act5).size should be(2)
			
		cp.add(resourceSet.resourcesOf(act5) == 0)
			
		act5.start.min should be(6)
	}
	
	test("Test 8: alternatives") {
		
		val horizon = 11
		val cp = new CPScheduler(horizon)	
		
		val act1 = Activity(cp, 0 to 5)
		val act2 = Activity(cp, 6)
		val acts = Array(act1, act2)
		
		val resourceSet = AlternativeCumulativeResource(cp, 2, 3)
		
		act1 needs (0 to 5) ofResources (0 to 1) in resourceSet
		act2 needs (0 to 5) ofResources (0 to 1) in resourceSet
		
		cp.add(act1.start == 0)

		cp.solveAll subjectTo {

			cp.add(resourceSet.resourcesOf(act1) == 0)
			cp.add(resourceSet.heightOf(act1) == 2)
			cp.add(resourceSet.heightOf(act2) == 2)
			cp.add(resourceSet.resourcesOf(act2) == 0)
			cp.add(act2.start == 3)
		}
		
		act1.dur.min should be(0)
		act1.dur.max should be(3)
	}
	
	test("Test 9: alternatives") {	
		
		val horizon = 106
		val cp = new CPScheduler(horizon)	
		
		val act1 = Activity(cp, 6)
		val act2 = Activity(cp, 6)
		val act3 = Activity(cp, 6)
		val act4 = Activity(cp, 6)
		val act5 = Activity(cp, 6)
		
		val resource1 = MaxResource(cp, 5)
		val resource2 = MaxResource(cp, 5)
		
		// Pool of the two previous resources
		val resourceSet = AlternativeCumulativeResource(resource1, resource2)
		
		// OR needs
		act1 needs 1 ofResources (0 to 1) in resourceSet
		act3 needs 3 ofResources (0 to 1) in resourceSet
		act4 needs 2 ofResources (0 to 1) in resourceSet
		act5 needs 1 ofResources (0 to 1) in resourceSet
	
		// AND needs
		act2 needs 4 ofResource resource1
		
		cp.add(act1.start == 0)
		cp.add(act2.start == 0)
		cp.add(act3.start == 0)
		cp.add(act4.start == 0)  
		
		cp.addResourceConstraints()
		
		resourceSet.resourcesOf(act1).value should be(0)
		resourceSet.resourcesOf(act3).value should be(1)
		resourceSet.resourcesOf(act4).value should be(1)
		
		resourceSet.resourcesOf(act5).size should be(2)
			
		cp.add(resourceSet.resourcesOf(act5) == 0)
			
		act5.start.min should be(6)
	}
}

object TestCumulativeResourceSet extends App {
	
	
}