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

object CumulativeJobShopLNS extends CPModel {
  
	def main(args: Array[String]) {
		
		// Parsing		
		// -----------------------------------------------------------------------
		
		// Read the data
		var lines = Source.fromFile("data/cJobShopVeryHard.txt").getLines.toList
		
		val nJobs        = lines.head.trim().split(" ")(0).toInt 
		val nTasksPerJob = lines.head.trim().split(" ")(1).toInt
		val nMachines    = lines.head.trim().split(" ")(2).toInt
		val capacity     = lines.head.trim().split(" ")(3).toInt
		
		val Jobs     = 0 until nJobs
		val Machines = 0 until nMachines
		
		println("#Jobs      : " + nJobs)
		println("#Tasks/job : " + nTasksPerJob)
		println("#Machines  : " + nMachines)
		println("#Capacity  : " + capacity)
		
		lines = lines.drop(1)
		
		val machines   : Array[Array[Int]] = Array.fill(nJobs, nTasksPerJob)(0)
		val durations  : Array[Array[Int]] = Array.fill(nJobs, nTasksPerJob)(0)
		val capacities : Array[Int] = Array.fill(nTasksPerJob)(capacity);
		
		for (i <- Jobs) {
			
			val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
			
			println("job "+ (i+1) +"\t" + l.mkString(" "))
			
			machines(i)  = Array.tabulate(nTasksPerJob)(j => l(2*j))
			durations(i) = Array.tabulate(nTasksPerJob)(j => l(2*j+1))
	  	    lines = lines.drop(1)
		}
		
		// Upper bound of the horizon
		val horizon = durations.flatten.sum

		// Modeling	
		// -----------------------------------------------------------------------
		
		val cp = CPSolver()

		// Matrix of cumulative activities (each line represents a job)
		val jobActivities = Array.tabulate(nJobs, nTasksPerJob)((i,j) => {
			
  	   		val dur      = CPVarInt(cp, durations(i)(j))
  	   		val start    = CPVarInt(cp, 0 to horizon - dur.getMin())
  	   		
  	   		CumulativeActivity(start,dur, machines(i)(j), 1)
  	   	}) 	   
  	   	
  	   	val activities = jobActivities.flatten
  	   	
  	   	// The make span to minimize
  	   	val makespan = maximum(0 until nJobs)(i => jobActivities(i)(nTasksPerJob-1).end)
  	   	
  	   	// Visualization  
  	   	// -----------------------------------------------------------------------
  	   	 	
  	   	val frame = new VisualFrame("Cumulative Job-Shop Problem", nMachines+1, 1)
		
		val cols = VisualUtil.getRandomColorArray(nMachines)
		val visualActivities = activities.map(a => VisualActivity(a))
		
		// Gantt Chart
		val gantt = new VisualGanttChart(visualActivities, cols, _ / nTasksPerJob)
		frame.createFrame("Jobs").add(gantt)
		
		// Profiles 
		val profiles : Array[VisualProfile] = new Array(nTasksPerJob)
		
		for (i <- Machines) {
			
			profiles(i) = new VisualProfile(visualActivities, i, capacities(i), cols(i))
			frame.createFrame("Machine " + i).add(profiles(i))
		}
	   
		def updateVisu(xScale : Int, yScale : Int) = {
			
			gantt.update(xScale, yScale)
			
			for (i <- Machines)
			   profiles(i).update(xScale, yScale)
		}
		frame.pack
	   	
  	   	// Constraints and Solving
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
			
			cp.post(constraints)
		}
		
  	   	cp.minimize(makespan) subjectTo {
			
			// Precedence constraints
			for (i <- Jobs; j <- 0 until nTasksPerJob-1)
				cp.add(jobActivities(i)(j).end <= jobActivities(i)(j+1).start)
			
			// Cumulative constraints
			for (i <- Machines)
				cp.add(new MaxSweepCumulative(cp, activities, capacities(i), i))

		} exploration {
			
			//cp.binaryFirstFail(activities.map(_.start))
			
			// Efficient but not complete search strategy
			SchedulingUtils.setTimesSearch(cp, activities)
		
			println
			cp.printStats() 
			
			// Best so far solution
			for (t <- 0 until activities.size) {
				
				bestSol(t).start   = activities(t).start.getValue
				bestSol(t).end     = activities(t).end.getValue
				bestSol(t).inc     = activities(t).resource.getValue
				bestSol(t).machine = activities(t).machine.getValue
			}
			precedences = PartialOrderSchedule.getPrecedences(bestSol, capacities)
			
			// Updates the visual components
			updateVisu(1, 20)
		}    
		println
		cp.printStats() 
	}
}
