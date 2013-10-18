package oscar.cp.dsl.instances
import oscar.cp.modeling.CPScheduler
import oscar.cp.dsl.InstanceReader

class HybridReentrantSSIntance(filepath: String) extends InstanceReader(filepath) {
  
  val Array(nbJobs,nbMachines) = readLine asInt
  
  val tasksValues = 3		// Tasks are described by 3 values.
  
  val initTime = readLine asInt
  
  val Array(jobs, initTime, setupTime, processingTime) = readDatas(nbJobs, tasksValues) asInt
  
  val cp = CPScheduler(processingTime.sum)

}