/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

package oscar.examples.cp.scheduling

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.cp.scheduling._
import oscar.visual._
import scala.io.Source

/**
 *  Flexible Job Shop (not yet finished)
 *  
 *  
 *  
 *  @authors: Pierre Schaus  pschaus@gmail.com
 */
object JFSSP extends App {

	var lines = Source.fromFile("data/FJSSPinstances/1_Brandimarte/BrandimarteMk1.fjs").getLines.toList
	
	println(lines.head.trim().split("\t")(0))
	
	class Activity(val machines: Array[(Int,Int)]) // (machine,duration)

	val nJobs        = lines.head.trim().split("\t")(0).toInt
	val nMachines    = lines.head.trim().split("\t")(1).toInt
	
	println("#jobs:"+nJobs+" #machines:"+nMachines)
	for (j <- 1 to nJobs) {
	  println("line"+j+":"+lines(j).trim().split(" +").mkString("%"))
	  //  the first number is the number of operations of that job, the second number (let's say k>=1) is the number of machines that can process the first operation; then according to k, there are k pairs of numbers (machine,processing time) 
	  var acts = lines(j).trim().split(" +").drop(1).map(_.toInt)
	  def read(): Int = {
	    val r = acts(0)
	    acts = acts.drop(1)
	    r
	  }
	  val nAct = read() // nb acts in the jobs
	  /*
	  val Job = Array.tabulate(nAct) {
	    
	    //for (a <- 0 until nAct) {
	     // val nMachines = read
	      //Array.tabulate(nMachines)(i =>read(),read())
	      //for (m <- 0 until nMachines) {
	    	  
	      //}
	    //}
	    new Activity(Array(0),Array(0))
	  }
	  */
	}
	
	
	
	
	
}
