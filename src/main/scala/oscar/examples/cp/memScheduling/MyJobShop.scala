package oscar.examples.cp.memScheduling

import oscar.cp.modeling._
import oscar.cp.memScheduling._
import oscar.cp.memScheduling.instances._
import oscar.visual._
import oscar.cp.scheduling._



object MyJobShop extends App with Scheduler with Reader {
    
    // Parsing
	  read fromFile "data/memScheduling/jobshop/ft10"
	  val Array(nbJobs, nbMachines) = read fileFor 2 int
	  val Array(jobs, requirements, durations) = read fileFor nbJobs unitsOf 2 int
	  
	  // Modeling
	  this setHorizonTo durations.sum
	  val activities = Activities(durations)
	  val machines = UnitResources(nbMachines)
	  
	  activities needs requirements ofResources machines
	  
	  // Need something the like of "activities precedes precedences"
	  for (i <- 0 until activities.length - 1; if (jobs(i) == jobs(i + 1)))
				activities(i) precedes activities(i + 1)
	  //for (i <- 0 until activities.length) {
	  //  if(precedences(i) != -1) activities(i) precedes activities(precedences(i))
	  //}
	  
	  /*
	  // Visualization coding is hideous, but that's not a priority right now.
	  // Visualization  
		// -----------------------------------------------------------------------
	
		val frame = new VisualFrame("Job-Shop Problem", 2, 1)
		val colors = VisualUtil.getRandomColors(nbMachines, true)
	
		val gantt1 = new VisualGanttChart(activities, i => jobs(i), colors = i => colors(requirements(i)))
		val gantt2 = new VisualGanttChart(activities, i => requirements(i), colors = i => colors(requirements(i)))
	
		frame.createFrame("Gantt chart").add(gantt1)
		frame.createFrame("Resources utilization").add(gantt2)
		frame.pack
	  */
	  
	  // TODO: review
	  val makespan = maximum(activities)(_.end)
	  minimize(makespan) subjectTo {
	    
	  } exploration {
	
			for (r <- (0 until nbMachines).sortBy(-machines(_).criticality).suspendable) {
				machines(r).rank()
		}
			binary(Array(makespan))
			/*
			gantt1.update(1, 20)
			gantt2.update(1, 20)
			*/
		} run()
	
		printStats()
}