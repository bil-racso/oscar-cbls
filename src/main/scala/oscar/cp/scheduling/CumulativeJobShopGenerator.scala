package oscar.cp.scheduling

import scala.util.Random.nextInt
import scala.util.Random.shuffle

object CumulativeJobShopGenerator {
	
	def getInstance(nJobs : Int, nMachines : Int, nTasksPerJob : Int, capacity : Int, maxDuration : Int, fullMachine : Boolean) : Tuple3[Array[Array[Int]], Array[Array[Int]], Array[Int]] = {
		
		val minDuration = maxDuration/10
		
		val machines : Array[Array[Int]] =  
		if (fullMachine) 
			Array.fill(nJobs)(shuffle(0 until nMachines).toArray)
		else
			Array.fill(nJobs, nTasksPerJob)(nextInt(nMachines))
		
		val durations  : Array[Array[Int]] = Array.fill(nJobs, nTasksPerJob)(nextInt(maxDuration)+minDuration)
		val capacities : Array[Int] = Array.fill(nTasksPerJob)(capacity);
		
		println(nJobs + " " + nTasksPerJob + " " + nMachines + " " + capacity)
		for(i <- 0 until nJobs) {
			for (j <- 0 until nTasksPerJob)
				print(machines(i)(j) + " " + durations(i)(j) + " ")
			println
		}
		
		return (machines, durations, capacities)
	}
	
	def main(args : Array[String]) {
		
		val nJobs = 50
		val nMachines = 15
		val nTasksPerJob = 15
		val capacity = 2
		val maxDuration = 50
		
		getInstance(nJobs, nMachines, nTasksPerJob, capacity, maxDuration, true) 
	}
}