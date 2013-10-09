package oscar.cp.dsl.instances
import oscar.cp.modeling.CPScheduler
import oscar.cp.dsl.InstanceReader

class JobShopInstance(filepath: String) {
 
  val reader = new InstanceReader(filepath)
  
  val Array(nbJobs, nbMachines) = reader.readLine
  val Array(jobs, requirements, durations) = reader.readDatas(2)
  
  val cp = CPScheduler(durations.sum)
  
}