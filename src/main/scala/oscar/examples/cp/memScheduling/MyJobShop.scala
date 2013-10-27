package oscar.examples.cp.memScheduling

import oscar.cp.modeling._
import oscar.cp.memScheduling._



object MyJobShop extends App with Scheduler with Reader {
    
    // Parsing
	  read fromFile "data/memScheduling/jobshop/ft10"
	  val Array(nbJobs, nbMachines) = read fileFor 2 int
	  val Array(jobs, requirements, durations) = read fileFor nbJobs unitsOf 2 int
	  
	  // Modeling
	  horizon setTo durations.sum
	  val activities = Activities(durations)
	  val machines = UnitResources(nbMachines)
	  
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
	  
	  val makespan = maximum(activities.map(_.end))
	  scheduler minimize makespan subjectTo {
	    
	    activities needs requirements ofResources machines
		  // TODO
		  for (i <- 0 until activities.length - 1; if (jobs(i) == jobs(i + 1)))
					activities(i) precedes activities(i + 1)
	    
	  } exploration {
	  	// TODO
			for (r <- (0 until nbMachines).sortBy(-machines(_).criticality).suspendable) {
				machines(r).rank()
			}
			// TODO: explore binary? What's that binary actually?
			// Pas plus simple de repartir de leur nouvel version avec un simple first fail ?
			scheduler binary(Array(makespan))
			/*
			gantt1.update(1, 20)
			gantt2.update(1, 20)
			*/
		} run()
		
		scheduler printStats

}