package oscar.examples.userman.cp

import oscar.cp.modeling._
import oscar.cp.scheduling._

object FirstScheduling extends App{
	
	val horizon = 8
	val cp = CPScheduler(horizon)
	
	val act1 = Activity(cp, 2, "My first activity")
	val act2 = Activity(cp, 4, "My second activity")
	
	cp.solve subjectTo {	
		act1 precedes act2
		
	} exploration {
		cp.binary(cp.activities)
	}
	
	println(act1.name + " starts at " + act1.start)
	println(act2.name + " starts at " + act2.start)
}
