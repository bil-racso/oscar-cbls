package oscar.examples.cp.memScheduling

import oscar.cp.dsl.instances.CrewSchedulingInstance

object MyCrewScheduling extends CrewSchedulingInstance("data/memScheduling/crew-scheduling/csp50.txt") with App{
	
  // Testing parsed values
  println(timeLimit)
  for (t <- tasks) {
    print(tasks(t) + " " + starts(t) + " " + ends(t))
    println
  }
  for (a <- arcs) {
    print(arcs(a) + " " + i(a) + " " + j(a) + " " + costs(a))
    println
  }
  
  // TODO: Now... model this!

}