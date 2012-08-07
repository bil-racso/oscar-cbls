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

package oscar.examples.cp.scheduling

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.cp.scheduling._
import oscar.visual._
import scala.io.Source

/**
 * @author Pierre Schaus pschaus@gmail.com
 * 
 * Model for the classical nxn Job-Shop problem
 * 
 * A Job is a a sequence of n Activities that must be executed one after the others.
 * There are n machines and each activity of the jobs require one of the n machines.
 * The objective is to assign the starting time of each activity minimizing the total makespan and
 * such that no two activities from two different jobs requiring the same machine overlap.
 */
object JobShop {
	
	def main(args : Array[String]) {

		// Parsing		
		// -----------------------------------------------------------------------

		var lines = Source.fromFile("data/ft10.txt").getLines.toList

		val nJobs        = lines.head.trim().split(" ")(0).toInt
		val nTasksPerJob = lines.head.trim().split(" ")(1).toInt
		val nResources   = lines.head.trim().split(" ")(2).toInt

		val nActivities  = nJobs * nTasksPerJob

		val Activities   = 0 until nActivities
		val Jobs         = 0 until nJobs
		val Resources    = 0 until nResources

		println("#Jobs       : " + nJobs)
		println("#Activities : " + nActivities)
		println("#Resources  : " + nResources)

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
		val resources  = Array.tabulate(nResources)(m => UnitResource(cp))

		// Resource allocation
		for (i <- Activities)
			activities(i).needs(resources(machines(i)))

		// The makespan to minimize
		val makespan = maximum(0 until nActivities)(i => activities(i).end)

		// Visualization  
		// -----------------------------------------------------------------------

		val frame  = new VisualFrame("Cumulative JobShop Problem", 2, 1)
		val colors = VisualUtil.getRandomColorArray(nResources)
		
		val gantt1 = new VisualGanttChart(activities, i => jobs(i), colors = i => colors(machines(i)))
		val gantt2 = new VisualGanttChart(activities, i => machines(i), colors = i => colors(machines(i)))
		
		frame.createFrame("Gantt chart").add(gantt1)
		frame.createFrame("Resources utilization").add(gantt2)
		frame.pack

		// Constraints & Search
		// -----------------------------------------------------------------------

		cp.minimize(makespan) subjectTo {

			for (i <- 0 until nActivities - 1; if (jobs(i) == jobs(i + 1)))
				cp.add(activities(i) endBeforeStart activities(i + 1))

		} exploration {

			cp.binaryFirstFail(activities.map(_.start))
			//cp.setTimesSearch(activities)

			//for (p <- profiles) p.update(1, 20)
			gantt1.update(1, 20)
			gantt2.update(1, 20)
		}

		cp.printStats()
	}
}
	  
