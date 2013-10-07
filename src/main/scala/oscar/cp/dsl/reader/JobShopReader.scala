package oscar.cp.dsl.reader

import scala.io.Source
import scala.collection.mutable.ListBuffer

object JobShopReader {

  def apply(filepath: String): Tuple5[Int, Int, Array[Int], Array[Int], Array[Int]] = {
    
    val file = Source.fromFile(filepath).getLines
    def nextLineAsInt = file.next.trim.split(" +").map(_.toInt)
    
    val Array(nbJobs, nbMachines) = nextLineAsInt
    val jobs					=		new ListBuffer[Int]
    val requirements 	= 	new ListBuffer[Int]
    val durations	 		= 	new ListBuffer[Int]
    
    for (job <- 0 until nbJobs){
      for (Array(req, dur) <- nextLineAsInt grouped(2)){
      	jobs append job
      	requirements append req
      	durations append dur
      }
    }
    
    Tuple5(nbJobs, nbMachines, jobs toArray, requirements toArray, durations toArray)
  }
  
}