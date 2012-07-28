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
		
		val constraint1 = new MinSweepCumulative(cp, tasks, 4, 0)
		val constraint2 = new MinSweepCumulative(cp, tasks, 3, 1)
		
		cp.add(constraint1)
		cp.add(constraint2)
		
		t2.est should be(1)
		t2.lst should be(2)
		
		t2.ect should be(3)
		t2.lct should be(4)
		
		t2.minDuration should be(1)
		t2.maxDuration should be(2)
		
		t2.minResource should be(3)
		t2.maxResource should be(4)
	}
	
	
	test("Test1: alternatives") {
		
		val cp = CPSolver()
			
		val req = Array(1,4,3,2)
		val tasks = for ((r,i) <- req.zipWithIndex) yield {
		             CumulativeActivity(CPVarInt(cp, 0), // start
										CPVarInt(cp, 6), // duration
										CPVarInt(cp, 0 to 1), // machine
										CPVarInt(cp, r)) // resource
		}
		var nbSol = 0
		val expectedSol = Set((0,0,1,1),(1,1,0,0))
		cp.solveAll subjectTo {
		
		  cp.add(new MaxSweepCumulative(cp, tasks, 5, 0))
		  cp.add(new MaxSweepCumulative(cp, tasks, 5, 1))
		  cp.add(new MinSweepCumulative(cp, tasks, 5, 0))
		  cp.add(new MinSweepCumulative(cp, tasks, 5, 1))
	
		} exploration {
		  cp.binary(tasks.map(_.machine))
		  val sol = (tasks(0).machine.getValue(),tasks(1).machine.getValue(),tasks(2).machine.getValue(),tasks(3).machine.getValue())	
		  expectedSol.contains(sol) should be(true)
		  nbSol += 1
		}
		nbSol should be(2)
	}
	
	test("Test2: alternatives") {	
		val cp = CPSolver()		
		val req = Array(1,4,3,2)
		val tasks = for ((r,i) <- req.zipWithIndex) yield {
		             CumulativeActivity(CPVarInt(cp, 0), // start
										CPVarInt(cp, 6), // duration
										CPVarInt(cp, 0 to 1), // machine
										CPVarInt(cp, r)) // resource
		}
		var nbSol = 0
		cp.solveAll subjectTo {
		
		  cp.add(new MaxSweepCumulative(cp, tasks, 5, 0))
		  cp.add(new MaxSweepCumulative(cp, tasks, 5, 1))
		  cp.add(tasks(1).machine == 0)
		  
		  tasks(0).machine.getValue should be(0)
		  tasks(2).machine.getValue should be(1)
		  tasks(3).machine.getValue should be(1)
	
		} exploration {
		  cp.binary(tasks.map(_.machine))
		  nbSol += 1
		}
		nbSol should be(1)
	}
	
	test("Test3: MaxCum") {	
		val cp = CPSolver()	
		val req = Array(1,4,3,2)
		val tasks = for ((r,i) <- req.zipWithIndex) yield {
		             CumulativeActivity(CPVarInt(cp, 0), // start
										CPVarInt(cp, 6), // duration
										CPVarInt(cp, 0 to 1), // machine
										CPVarInt(cp, r)) // resource
		}
		val myAct = CumulativeActivity(CPVarInt(cp, 0 to 100), // start
					            	   CPVarInt(cp, 6), // duration
						               CPVarInt(cp, 0 to 1), // machine
						               CPVarInt(cp, 1))

		cp.solveAll subjectTo {
		
		  cp.add(new MaxSweepCumulative(cp, tasks :+ myAct, 5, 0))
		  cp.add(new MaxSweepCumulative(cp, tasks :+ myAct, 5, 1))
		  cp.add(tasks(1).machine == 0)
		  
		  tasks(0).machine.getValue should be(0)
		  tasks(2).machine.getValue should be(1)
		  tasks(3).machine.getValue should be(1)
		  myAct.machine.getSize() should be(2)
		  cp.add(myAct.machine == 0)
		  myAct.start.getMin() should be(6)
	
		}
	}
	
	test("Test4: MaxCum") {	
		val cp = CPSolver()		

		val myAct1 = CumulativeActivity(CPVarInt(cp, 0), // start
					            	    CPVarInt(cp, 6), // duration
						                CPVarInt(cp, 0 to 1), // machine
						                CPVarInt(cp, 0 to 5)) // consumption

		val myAct2 = CumulativeActivity(CPVarInt(cp, 0), // start
					            	    CPVarInt(cp, 6), // duration
						                CPVarInt(cp, 0 to 1), // machine
						                CPVarInt(cp, -2 to 5)) // consumption						               
		cp.solveAll subjectTo {
		
		  cp.add(new MaxSweepCumulative(cp, Array(myAct1,myAct2), 3, 0))
		  
		  cp.add(myAct1.machine == 0)
		  cp.add(myAct2.machine == 0)
		  cp.add(myAct1.resource == 5) // it means myAct2 must have a negative consumption of -2
		  
		  myAct2.resource.getValue should be(-2)
		 
		}
	}
	
	test("Test5: MaxCum") {	
		val cp = CPSolver()		

		val myAct1 = CumulativeActivity(CPVarInt(cp, 0), // start
					            	    CPVarInt(cp, 0 to 5), // duration
						                CPVarInt(cp, 0 to 1), // machine
						                CPVarInt(cp, 0 to 5)) // consumption

		val myAct2 = CumulativeActivity(CPVarInt(cp, 0 to 5), // start
					            	    CPVarInt(cp, 6), // duration
						                CPVarInt(cp, 0 to 1), // machine
						                CPVarInt(cp, 0 to 5)) // consumption						               
		cp.solveAll subjectTo {
		
		  cp.add(new MaxSweepCumulative(cp, Array(myAct1,myAct2), 3, 0))
		  
		  cp.add(myAct1.machine == 0)
		  cp.add(myAct1.resource == 2)
		  cp.add(myAct2.resource == 2)
		  cp.add(myAct2.machine == 0)
		  cp.add(myAct2.start == 3)
		  
		  myAct1.dur.getMin() should be(0)
		  myAct1.dur.getMax() should be(3)

		}
	}	
	
}

