package oscar.examples.cp.memScheduling

import oscar.cp.memScheduling._

object MyCrewScheduling extends App with Scheduler with Reader {
  
  read fromFile "data/memScheduling/crew-scheduling/csp50.txt"
  val Array(nbTasks, timeLimit) 	= read fileFor 2 int
  val Array(tasks, starts, ends) 	= read fileFor nbTasks unitsOf 2 int
  val Array(arcs, i, j, costs) 		= read fileFor allRemaining unitsOf 3 int
	
  // Testing parsed values
  println(nbTasks)
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