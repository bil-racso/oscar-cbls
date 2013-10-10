package oscar.cp.dsl.instances

import oscar.cp.dsl.InstanceReader
import math.round

class CommonDueDateInstance(filepath: String) extends InstanceReader(filepath){

  val Array(nbJobs) = readLine asInt
  
  val jobsValues = 3		// Jobs are described by 3 values.
  
  val Array(jobs, processingTimes, earliness, tardiness) = readDatas(nbJobs, jobsValues) asInt

  val h = 0.8		// External parameter to set a more or less restrictive due date, to be set by user.
  val dueDate = (processingTimes.sum * h) toInt
  

}