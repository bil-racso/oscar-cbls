package oscar.cp.dsl

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.cp.scheduling._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

/*
case class JobShop(cp: CPScheduler, activities: Array[Activity], jobs: Array[Int], requirements: Array[Int], resources: Array[UnitResource]) {
  
  def customize(function: => Unit){
    function
  }
  
}*/

class JobShop(filepath: String) {
  
  //def apply(filepath: String): JobShop = {
    
    // Parsing
    // ----------------------------------------------------------------------
    val file = Source.fromFile(filepath).getLines
    def nextLineAsInt = file.next.trim.split(" +").map(_.toInt)
    
    val Array(nbJobs, nbMachines) = nextLineAsInt
    val jobs					=		new ArrayBuffer[Int]
    val requirements 	= 	new ArrayBuffer[Int]
    val durations	 		= 	new ArrayBuffer[Int]
  
	for (job <- 0 until nbJobs){
		for (Array(req, dur) <- nextLineAsInt.grouped(2)){
			jobs += job
  			requirements += req
  			durations += dur
		}
	}
  
	// Modeling	
    // ----------------------------------------------------------------------
		
	val horizon = durations.sum
	val cp = new CPScheduler(horizon)
	
	// Activities & Resources
	val activities = Array.tabulate(requirements.length)(i => Activity(cp, durations(i)))
	val resources  = Array.tabulate(nbMachines)(r => UnitResource(cp))
	val makespan = maximum(activities)(_.end)
	  
	// Constraints
	def defaultConstraints {
	// Resource allocation
		for (i <- 0 until activities.length) 
	  		activities(i) needs resources(requirements(i))
		  
		// Precedence constraints
		for (i <- 0 until activities.length - 1; if (jobs(i) == jobs(i + 1)))
		      activities(i) precedes activities(i + 1)
	  }
	  
	  //new JobShop(cp, activities, jobs, requirements, resources)
	//}
	  
}