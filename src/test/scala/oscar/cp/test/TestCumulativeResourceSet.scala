package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._
import oscar.cp.scheduling._

import org.scalacheck._

class TestCumulativeResourceSet extends FunSuite with ShouldMatchers {
	
	test("Test 1: alternatives") {	
		
		val horizon = 6
		val cp = new CPScheduler(horizon)		
		val req = Array(1,4,3,2)
		
		val act1 = Activity(cp, 6)
		val act2 = Activity(cp, 6)
		val act3 = Activity(cp, 6)
		val act4 = Activity(cp, 6)
		val acts = Array(act1, act2, act3, act4)
		
		val resource = CumulativeResourceSet(cp, 2, 5)
		
		act1.needs(resource, Array(0, 1), 1)
		act2.needs(resource, Array(0, 1), 4)
		act3.needs(resource, Array(0, 1), 3)
		act4.needs(resource, Array(0, 1), 2)
		
		var nbSol = 0

		cp.solveAll 
		cp.subjectTo {
			
			cp.add(resource.resourcesOf(act2) == 0)
		} 
		
		resource.resourcesOf(act2).value should be(0)
		resource.resourcesOf(act3).value should be(1)
		resource.resourcesOf(act4).value should be(1)
		
		cp.exploration {
			
			cp.binary(acts.map(resource.resourcesOf(_)))
			nbSol += 1
		}	
		
		nbSol should be(1)
	}
	
	test("Test 2: MaxCum") {	
		
		val horizon = 106
		val cp = new CPScheduler(horizon)	
		
		val act1 = Activity(cp, 6)
		val act2 = Activity(cp, 6)
		val act3 = Activity(cp, 6)
		val act4 = Activity(cp, 6)
		val act5 = Activity(cp, 6)
		val acts = Array(act1, act2, act3, act4, act5)
		
		val resource = CumulativeResourceSet(cp, 2, 5)
		
		act1.needs(resource, Array(0, 1), 1)
		act2.needs(resource, Array(0, 1), 4)
		act3.needs(resource, Array(0, 1), 3)
		act4.needs(resource, Array(0, 1), 2)
		act5.needs(resource, Array(0, 1), 1)

		cp.solveAll subjectTo {
		
			cp.add(act1.start == 0)
			cp.add(act2.start == 0)
			cp.add(act3.start == 0)
			cp.add(act4.start == 0)  
			  
			cp.add(resource.resourcesOf(act2) == 0)	
		}
		
		resource.resourcesOf(act1).value should be(0)
		resource.resourcesOf(act3).value should be(1)
		resource.resourcesOf(act4).value should be(1)
		
		resource.resourcesOf(act5).size  should be(2)
			
		cp.add(resource.resourcesOf(act5) == 0)
			
		act5.start.min should be(6)
	}
	
	test("Test 3: MaxCum") {
		
		val horizon = 11
		val cp = new CPScheduler(horizon)	
		
		val act1 = Activity(cp, 0 to 5)
		val act2 = Activity(cp, 6)
		val acts = Array(act1, act2)
		
		val resource = CumulativeResourceSet(cp, 2, 3)
		
		act1.needs(resource, Array(0, 1), 0 to 5)
		act2.needs(resource, Array(0, 1), 0 to 5)
		
		cp.add(act1.start == 0)

		cp.solveAll subjectTo {

			cp.add(resource.resourcesOf(act1) == 0)
			cp.add(resource.heightOf(act1) == 2)
			cp.add(resource.heightOf(act2) == 2)
			cp.add(resource.resourcesOf(act2) == 0)
			cp.add(act2.start == 3)
		}
		
		act1.dur.min should be(0)
		act1.dur.max should be(3)
	}
}