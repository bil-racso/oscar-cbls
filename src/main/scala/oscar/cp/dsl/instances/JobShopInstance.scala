package oscar.cp.dsl.instances
import oscar.cp.modeling.CPScheduler
import oscar.cp.dsl.InstanceReader

class JobShopInstance(filepath: String) extends InstanceReader(filepath) {
  
  val Array(nbJobs, nbMachines) = readLine asInt
  
  val tasksValues = 2		// Tasks are described by 2 values.
  
  val Array(jobs, requirements, durations) = readDatas(nbJobs, tasksValues) asInt
  
  val cp = CPScheduler(durations.sum)
  
}