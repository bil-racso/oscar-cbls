package oscar.cp.test.scheduling

import oscar.cp.modeling._
import oscar.cp.scheduling._
import oscar.visual._
import oscar.cp.dsl.instances.JobShopInstance

object MyJobShop extends JobShopInstance("data/memScheduling/jobshop/ft10") with App {
  
  // Well we actually need all those functions defined on arrays instead of just elements themselves...
  // Couldn't the activities and machines definitions go into the instance like makespan?
  // We need to find a way to get rid of that cp in the functions.
  
  // Need something the like of "Activities(cp, duration: Array[Int])" because using tabulate is hideous!
  val activities = Array.tabulate(durations.length)(i => Activity(cp, durations(i)))
  
  // Need something the like of "UnitResources(cp, nbMachines)", because... well, tabulate again.
  val machines = Array.tabulate(nbMachines)(i => UnitResource(cp))
  
  // Need something the like of "activities needs requirements machines"
  for (i <- 0 until activities.length) {
    activities(i) needs machines(requirements(i))
  }
  
  // Need something the like of "activities precedes precedences"
  for (i <- 0 until activities.length - 1; if (jobs(i) == jobs(i + 1)))
			activities(i) precedes activities(i + 1)
  //for (i <- 0 until activities.length) {
  //  if(precedences(i) != -1) activities(i) precedes activities(precedences(i))
  //}
  
  
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
  
  // TODO: review
  val makespan = maximum(activities)(_.end)
  cp.minimize(makespan) subjectTo {
    
  } exploration {

		for (r <- (0 until nbMachines).sortBy(-machines(_).criticality).suspendable) {
			machines(r).rank()
		}
		cp.binary(Array(makespan))
		gantt1.update(1, 20)
		gantt2.update(1, 20)
	} run()

	cp.printStats()
}