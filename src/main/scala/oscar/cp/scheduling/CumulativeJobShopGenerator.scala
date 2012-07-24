package oscar.cp.scheduling

import scala.util.Random.nextInt
import scala.util.Random.shuffle

object CumulativeJobShopGenerator {
	
	def getInstance(nJobs : Int, nMachines : Int, nTasksPerJob : Int, capacity : Int, maxDuration : Int) : Tuple3[Array[Array[Int]], Array[Array[Int]], Array[Int]] = {
		
		val minDuration = maxDuration/10
		
		val machines   : Array[Array[Int]] = Array.fill(nJobs)(shuffle(0 until nMachines).toArray)
		val durations  : Array[Array[Int]] = Array.fill(nJobs, nTasksPerJob)(nextInt(maxDuration)+minDuration)
		val capacities : Array[Int] = Array.fill(nTasksPerJob)(capacity);
		
		return (machines, durations, capacities)
	}
}