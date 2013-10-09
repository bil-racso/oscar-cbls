package oscar.cp.dsl.instances
import oscar.cp.modeling.CPScheduler
import oscar.cp.dsl.InstanceReader

class JobShopInstance(filepath: String) extends InstanceReader(filepath) {
  
  val Array(nbJobs, nbMachines) = readLine asInt
  val Array(jobs, requirements, durations) = readDatas(2) asInt
  
  val cp = CPScheduler(durations.sum)
  
}