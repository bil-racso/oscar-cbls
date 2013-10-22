package oscar.cp.memScheduling.instances

import oscar.cp.memScheduling._

class SemiconductorTestingInstance(filepath: String) extends InstanceReader(filepath) {
  
  val Array(nbOperations, nbLots, nbMachines) = readLine asInt
  
  val operationsValues = 6		// Operations are described by 6 values.
  
  val Array(
      	operations, 
      	operationNumbers, 
      	lotNumbers, 
      	predecessors, 
      	jobOperationTime, 
      	jobDueDate,
      	requiredMachines
  		) = readDatas(nbOperations, operationsValues) asInt
  		
  val setupTimesValues = 3	// Setup times are described by 3 values.
  
  val Array(setupTimeArcs, i, j, setupTimes) = readDatas(allRemaining, setupTimesValues) asInt

}