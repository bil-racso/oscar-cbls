package oscar.cp.dsl

import oscar.visual._
import oscar.cp.modeling._

object MyJobShop extends JobShop("data/memScheduling/jobshop/ft10") with App {

  	// Visualization  
  	// -----------------------------------------------------------------------

	  val frame = new VisualFrame("Job-Shop Problem", 2, 1)
	  val colors = VisualUtil.getRandomColors(resources.length, true)
	  
	  val gantt1 = new VisualGanttChart(activities, i => jobs(i), colors = i => colors(requirements(i)))
	  val gantt2 = new VisualGanttChart(activities, i => requirements(i), colors = i => colors(requirements(i)))
	
	  frame.createFrame("Gantt chart").add(gantt1)
	  frame.createFrame("Resources utilization").add(gantt2)
	  frame.pack
	  
	  
	  // The makespan to minimize
	  cp.minimize(makespan) subjectTo defaultConstraints
	  
	  cp.exploration {
	    for (r <- (0 until resources.length).sortBy(-resources(_).criticality).suspendable) {
	      resources(r).rank()
	    }
	  
	    cp.binary(Array(makespan))
	    gantt1.update(1, 20)
	    gantt2.update(1, 20)
	  
	  } run()
	
	  cp.printStats()
}