package oscar.cp.dsl.instances

import oscar.cp.dsl.InstanceReader

class CrewSchedulingInstance(filepath: String) extends InstanceReader(filepath) {
  
  val Array(nbTasks, timeLimit) = readLine asInt
  
  val taskValues = 2	// Tasks are described by 2 value.
  val Array(tasks, starts, ends) = readDatas(nbTasks, taskValues) asInt
  
  val arcValues = 3 	// Transition arcs are described by 3 values.
  val Array(arcs, i, j, costs) = readDatas(allRemaining, arcValues) asInt

}