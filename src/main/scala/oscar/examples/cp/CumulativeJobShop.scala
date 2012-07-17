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

package oscar.examples.cp

import oscar.cp.constraints.MaxCumulative
import oscar.cp.constraints.NaiveMultiCumulative
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.scheduling._
import oscar.reversible.ReversibleSetIndexedArray
import oscar.reversible.ReversibleInt
import oscar.search._
import oscar.visual._

import scala.io.Source

object CumulativeJobShop extends CPModel {
  
	def main(args: Array[String]) {
	  
		// Parsing		
		// -----------------------------------------------------------------------
		
		// Read the data
		var lines = Source.fromFile("data/cJobShopHard.txt").getLines.toList
		
		val nJobs        = lines.head.trim().split(" ")(0).toInt 
		val nTasksPerJob = lines.head.trim().split(" ")(1).toInt
		val nMachines    = nTasksPerJob
		
		val Jobs        = 0 until nJobs
		val Machines    = 0 until nMachines
		
		val capacity = lines.head.trim().split(" ")(2).toInt
		
		println("#Jobs      : " + nJobs)
		println("#Tasks/job : " + nTasksPerJob)
		println("#Machines  : " + nMachines)
		println("#Capacity  : " + capacity)
		
		lines = lines.drop(1)
		
		val machines   : Array[Array[Int]] = Array.fill(nJobs, nMachines)(0)
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
  	   		val start    = CPVarInt(cp,0 to horizon - dur.getMin())
  	   		
  	   		CumulativeActivity(start,dur, machines(i)(j), 1)
  	   	}) 	   
  	   	
  	   	// The make span to minimize
  	   	val makespan = maximum(0 until nJobs)(i => jobActivities(i)(nTasksPerJob-1).getEnd)
  	   	
  	   	// Visualization  
  	   	// -----------------------------------------------------------------------
  	   	 	
  	   	val frame = new VisualFrame("Cumulative Job-Shop Problem",nMachines+1,1)
		
		val cols = VisualUtil.getRandomColorArray(nMachines)
		val visualActivities = jobActivities.flatten.map(a => VisualActivity(a))
		
		// Gantt Chart
		val gantt = new VisualGanttChart(visualActivities, nTasksPerJob, cols)
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

  	   	cp.minimize(makespan) subjectTo {

			// Precedence constraints
			for (i <- Jobs; j <- 0 until nTasksPerJob-1) {
				cp.add(jobActivities(i)(j).getEnd() <= jobActivities(i)(j+1).getStart())
			}
			
			// Cumulative constraints
			for (i <- Machines) {
				cp.add(new MaxCumulative(cp, jobActivities.flatten, capacities(i), i))
			}

		} exploration {
			
			// Efficient but not complete search strategy
			cp.binaryFirstFail(jobActivities.flatten.map(_.getStart))
			//SchedulingUtils.setTimesSearch(cp, jobActivities.flatten)
			//cp.binaryFirstFail(jobActivities.flatten.map(a => a.getStart()))
			
			
			println("start times:"+jobActivities.flatten.map(_.getStart()).mkString(","))
			// Updates the visual components
			updateVisu(1, 20)
		}
		
		cp.printStats()       
	}
}
