package oscar.examples.cp.scheduling

import scala.io.Source

class JobShopInstance(val nbJobs: Int, val nbActPerJob: Int, val jobMatrix: Array[Array[Int]], val durationMatrix: Array[Array[Int]])
object JobShopParser {
  
  def parse(f: String): JobShopInstance = {
      	// Parsing		
	// -----------------------------------------------------------------------

	var lines = Source.fromFile("data/jobshop/Lawrence/la01.txt").getLines.toList
	while (lines.head.trim().startsWith("+++") || lines.head.trim().isEmpty()) {
	  lines = lines.drop(1)
	}
	println(lines.head)
	lines = lines.drop(1)
	val nJobs        = lines.head.trim().split(" ")(0).toInt
	val nTasksPerJob = lines.head.trim().split(" ")(1).toInt
    lines = lines.drop(1)
    val durations : Array[Array[Int]] = Array.fill(nJobs,nTasksPerJob)(0)
    val machines : Array[Array[Int]] = Array.fill(nJobs,nTasksPerJob)(0)
    for (i <- 0 until nJobs) {
                   val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
                   println(nTasksPerJob+" "+nJobs+" "+l.mkString(" ")+"  size: "+l.size)
                   machines(i) = Array.tabulate(nTasksPerJob)(j => l(2*j))
                   durations(i) = Array.tabulate(nTasksPerJob)(j => l(2*j+1))
                   lines = lines.drop(1)
    }
	new JobShopInstance(nJobs,nTasksPerJob,machines,durations)
  }
  
  def main(args: Array[String]) {
	  val js = parse("data/jobshop/Lawrence/la01.txt")
  }
  


}