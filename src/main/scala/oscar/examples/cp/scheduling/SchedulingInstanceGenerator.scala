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
	
	/** 
	 * State JOB-SHOP
	 */
	def stateJobShop(nJobs : Int, nTasksPerJob : Int, duration : Int) : Array[Array[Int]] = {
		
		val minDuration = duration/5
		val maxDuration = duration - minDuration
		
		val instance = new Array[Array[Int]](nJobs*nTasksPerJob)
		
		for (i <- 0 until nJobs) {
			
			val states    = shuffle(0 until nTasksPerJob).toArray
			val durations = Array.fill(nTasksPerJob)(nextInt(maxDuration)+minDuration)
			
			for (j <- 0 until nTasksPerJob) {
				
				instance(i*nTasksPerJob + j) = new Array[Int](3)
				
				instance(i*nTasksPerJob + j)(0) = i
				instance(i*nTasksPerJob + j)(1) = states(j)
				instance(i*nTasksPerJob + j)(2) = durations(j)
			}
		}
		
		return instance
	}
	
	def main(args : Array[String]) {
		
		val inst = stateJobShop(20, 5, 100)
		for (i <- 0 until 100) {
			println(inst(i).mkString(" "))
		}
	}
}