package oscar.cp.memScheduling.instances
import oscar.cp.modeling.CPScheduler
import oscar.cp.memScheduling._

class JobShopInstance(filepath: String) extends InstanceReader(filepath) {
  
  val Array(nbJobs, nbMachines) = readLine asInt
  
  val tasksValues = 2		// Tasks are described by 2 values.
  
  val Array(jobs, requirements, durations) = readDatas(nbJobs, tasksValues) asInt
  
  //implicit var cp_trololo:Option[CPScheduler] = None
  
  //cp_trololo = Some(CPScheduler(1))  
  
  val cp = CPScheduler(durations.sum)
  
}