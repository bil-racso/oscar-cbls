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

import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.scheduling._
import oscar.reversible.ReversibleSetIndexedArray
import oscar.reversible.ReversibleInt
import oscar.search._
import oscar.visual._
import scala.util.Random.nextFloat

import scala.io.Source
import scala.collection.mutable.Set

object CumulativeJobShopLNS {
	
	def main(args : Array[String]) {

		// Parsing		
		// -----------------------------------------------------------------------

		var lines = Source.fromFile("data/cJobShop.txt").getLines.toList

		val nJobs        = lines.head.trim().split(" ")(0).toInt
		val nTasksPerJob = lines.head.trim().split(" ")(1).toInt
		val nResources   = lines.head.trim().split(" ")(2).toInt
		val capacity     = lines.head.trim().split(" ")(3).toInt

		val nActivities  = nJobs * nTasksPerJob

		val Activities   = 0 until nActivities
		val Jobs         = 0 until nJobs
		val Resources    = 0 until nResources

		println("#Jobs       : " + nJobs)
		println("#Activities : " + nActivities)
		println("#Resources  : " + nResources)
		println("Capacity    : " + capacity)

		lines = lines.drop(1)

		val jobs = new Array[Int](nActivities)
		val machines = new Array[Int](nActivities)
		val durations = new Array[Int](nActivities)

		for (i <- Activities) {

			val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray

			jobs(i) = l(0)
			machines(i) = l(1)
			durations(i) = l(2)

			lines = lines.drop(1)
		}

		// Modeling	
		// -----------------------------------------------------------------------

		val horizon = durations.sum
		val cp = new CPScheduler(horizon)

		// Activities & Resources
		val activities = Array.tabulate(nActivities)(i => Activity(cp, durations(i)))
		val resources  = Array.tabulate(nResources)(m => CumulativeResource(cp, 2))

		// Resource allocation
		for (i <- Activities)
			activities(i) needs 1 ofResource resources(machines(i))

		// The makespan to minimize
		val makespan = maximum(0 until nActivities)(i => activities(i).end)

		// Visualization  
		// -----------------------------------------------------------------------

		val frame  = new VisualFrame("Cumulative JobShop Problem", nResources+1, 1)
		val colors = VisualUtil.getRandomColorArray(nResources)
		
		val gantt  = new VisualGanttChart(activities, i => jobs(i), colors = i => colors(machines(i)))
		val profiles = Array.tabulate(nResources)(i => new VisualProfile(resources(i), makespan, color = colors(i)))
		
		frame.createFrame("Gantt chart").add(gantt)
		for (p <- profiles) frame.createFrame(p.resource.toString).add(p)
		frame.pack

		// Constraints & Search
		// -----------------------------------------------------------------------
		
		val bestSol : Array[FixedActivity] = Array.tabulate(activities.size)(i => new FixedActivity(i, 0, 0, 0, 0))
		var precedences : Array[Tuple2[Int, Int]] = null
		
		cp.lns(200,300) {		
			
			val selected : Array[Boolean] = Array.fill(bestSol.size)(false)
			
			// Selected are relaxed (30%)
			for (i <- 0 until bestSol.size)
				if (nextFloat < 0.3)
					selected(i) = true
			
			val filteredPrecedences = precedences.filter(p => !selected(p._1) && !selected(p._2))
			val constraints = filteredPrecedences.map(p => activities(p._1).end <= activities(p._2).start)
			
			cp.post(constraints.asInstanceOf[Array[Constraint]])
		}

		cp.minimize(makespan) subjectTo {
			
			for (i <- 0 until nActivities - 1; if (jobs(i) == jobs(i + 1)))
				activities(i) precedes activities(i + 1)
				
		} exploration {

			cp.binaryFirstFail(activities.map(_.start))
			//cp.setTimesSearch(activities)
			
			// Best so far solution
			for (t <- Activities) {
				
				bestSol(t).start   = activities(t).est
				bestSol(t).end     = activities(t).lct
				bestSol(t).inc     = 1
				bestSol(t).machine = machines(t)
			}
			
			precedences = PartialOrderSchedule.getPrecedences(bestSol, Array.fill(nResources)(2))

			for (p <- profiles) p.update(1, 20)
			gantt.update(1, 20)
		}

		cp.printStats()
	}
}

