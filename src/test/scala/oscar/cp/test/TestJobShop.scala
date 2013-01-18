/*******************************************************************************
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
 ******************************************************************************/

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.cp.scheduling._
import oscar.visual._
import scala.io.Source


class TestJobShop extends FunSuite with ShouldMatchers  {
  
  
  test("jobshop") { 
    // Parsing		
	// -----------------------------------------------------------------------
	 /*
	var lines = Source.fromFile("data/ft06.txt").getLines.toList
    println(lines.mkString(","))
	val nJobs        = lines.head.trim().split(" ")(0).toInt
	val nTasksPerJob = lines.head.trim().split(" ")(1).toInt
	val nResources   = lines.head.trim().split(" ")(2).toInt

	val nActivities = nJobs * nTasksPerJob

	val Activities = 0 until nActivities
	val Jobs       = 0 until nJobs
	val Resources  = 0 until nResources

	lines = lines.drop(1)

	val jobs      = new Array[Int](nActivities)
	val machines  = new Array[Int](nActivities)
	val durations = new Array[Int](nActivities)

	for (i <- Activities) {
		val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
		jobs(i)      = l(0)
		machines(i)  = l(1)
		durations(i) = l(2)
		lines = lines.drop(1)
	}

	// Modeling	
	// -----------------------------------------------------------------------

	val horizon = durations.sum
	val cp = new CPScheduler(horizon)

	// Activities & Resources
	val activities = Array.tabulate(nActivities)(i => Activity(cp, durations(i)))
	val resources  = Array.tabulate(nResources)(r => UnitResource(cp))

	// Resource allocation
	for (i <- Activities) 
		activities(i) needs resources(machines(i))

	// The makespan to minimize
	val makespan = maximum(activities)(_.end)

	// Constraints & Search
	// -----------------------------------------------------------------------

	cp.minimize(makespan) subjectTo {

		for (i <- 0 until nActivities - 1; if (jobs(i) == jobs(i + 1)))
			activities(i) precedes activities(i + 1)

	} exploration {

		for (r <- (0 until nResources).sortBy(-resources(_).criticality).suspendable) {
			resources(r).rank()
		}
		cp.binary(Array(makespan))
	
	}

	cp.printStats()
	*/

  }  
  

 


}
