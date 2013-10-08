package oscar.cp.dsl.reader

import scala.io.Source
import scala.collection.mutable.ListBuffer
import oscar.cp.modeling.CPScheduler

class JobShopInstance(filepath: String) {
 
  val (nbJobs, nbMachines, jobs, precedences, requirements, durations) = JobShopReader("data/memScheduling/jobshop/ft10")
  val cp = CPScheduler(durations.sum)
  
}

object JobShopReader {

  def apply(filepath: String): Tuple6[Int, Int, Array[Int], Array[Int], Array[Int], Array[Int]] = {
    
    val file = Source.fromFile(filepath).getLines
    def nextLineAsInt = file.next.trim.split(" +").map(_.toInt)
    
    val Array(nbJobs, nbMachines) = nextLineAsInt
    val jobs					=		new ListBuffer[Int]
    val precedences 	=		new ListBuffer[Int]
    val requirements 	= 	new ListBuffer[Int]
    val durations	 		= 	new ListBuffer[Int]
    
    for (job <- 0 until nbJobs){
      val tasks = nextLineAsInt.grouped(2)
      for (Array(req, dur) <- tasks){
        jobs append job
      	precedences append (if (tasks.hasNext) precedences.length+1 else -1)
      	requirements append req
      	durations append dur
      }
    }
    
    Tuple6(nbJobs, nbMachines, jobs toArray, precedences toArray, requirements toArray, durations toArray)
  }
  
}