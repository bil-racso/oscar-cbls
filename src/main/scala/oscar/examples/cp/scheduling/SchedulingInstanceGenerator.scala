package oscar.examples.cp.scheduling

import scala.util.Random.nextInt
import scala.util.Random.shuffle
import scala.util.Random.nextBoolean

import scala.collection.mutable.Queue

object SchedulingInstanceGenerator {
	
	/** 
	 * CUMULATIVE JOB-SHOP
	 */
	def cumulativeJobShop(nJobs : Int, nMachines : Int, nTasksPerJob : Int, capacity : Int, maxDuration : Int, fullMachine : Boolean) : Tuple3[Array[Array[Int]], Array[Array[Int]], Array[Int]] = {
		
		val minDuration = maxDuration/10
		
		val machines : Array[Array[Int]] =  
		if (fullMachine) 
			Array.fill(nJobs)(shuffle(0 until nMachines).toArray)
		else
			Array.fill(nJobs, nTasksPerJob)(nextInt(nMachines))
		
		val durations  : Array[Array[Int]] = Array.fill(nJobs, nTasksPerJob)(nextInt(maxDuration)+minDuration)
		val capacities : Array[Int] = Array.fill(nTasksPerJob)(capacity);
		
		return (machines, durations, capacities)
	}
}