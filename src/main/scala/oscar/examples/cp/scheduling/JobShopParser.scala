/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.examples.cp.scheduling

import scala.io.Source

class JobShopInstance(val nbJobs: Int, val nbActPerJob: Int, val jobMatrix: Array[Array[Int]], val durationMatrix: Array[Array[Int]])
object JobShopParser {
  
  def parse(f: String): JobShopInstance = {
      	// Parsing		
	// -----------------------------------------------------------------------

	var lines = Source.fromFile(f).getLines.toList
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
