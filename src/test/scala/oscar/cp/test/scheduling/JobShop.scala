package oscar.cp.test.scheduling

import oscar.cp.dsl.reader.JobShopReader
import oscar.cp.modeling.CPScheduler
import oscar.cp.scheduling.Activity
import oscar.cp.scheduling.UnitResource

class JobShop {
   
  val (nbJobs, nbMachines, jobs, requirements, durations) = JobShopReader("/data/memScheduling/jobshop/ft10")
  
  val cp = CPScheduler(durations.sum)
  
  // Need something the like of "Activities(cp, duration: Array[Int])" because using tabulate is hideous!
  val activities = Array.tabulate(durations.length)(i => Activity(cp, durations(i)))
  // Need something the like of "UnitResources(cp, nbMachines)", because... well, tabulate again.
  val machines = Array.tabulate(nbMachines)(i => UnitResource(cp))
  
  // Need something the like of "activities needs requirements machines
  for (i <- 0 until activities.length) {
    activities(i) needs machines(requirements(i))
  }

}