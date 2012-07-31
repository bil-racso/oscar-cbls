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

import oscar.cp.constraints._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.scheduling._
import oscar.reversible.ReversibleSetIndexedArray
import oscar.reversible.ReversibleInt
import oscar.search._
import oscar.visual._

import scala.io.Source

object MachineSchedulingVariableSpeed {
  
	/**
	 * Given 7 tasks. Each task has to run on a given machine chosen from a set of 
	 * machines. Each machine can only run one task at a time and each task should 
	 * not be interrupted. The effective duration of a task is now computed by 
	 * multiplying the "duration" by a fixed coefficient (2 of M1, 1 of M2, 3 for 
	 * M3). Find the earliest end.
	 */
	
	def main(args: Array[String]) {
	  
		// Data		
		// -----------------------------------------------------------------------
		
		val nTasks    = 100
		val nMachines = 5
		
		val Tasks    = 0 until nTasks
		val Machines = 0 until nMachines
		
		val oDurations  : Array[Int] = Array(2, 1, 4, 2, 3, 1)
		val coefficient : Array[Int] = Array(2, 1, 3)
		val machines    : Array[Set[Int]] = Array(Set(0, 1), Set(0, 2), Set(0, 2), Set(1, 2), Set(0), Set(0, 2))
		
		val durations = Array.tabulate(nTasks)(i => coefficient.map(_*oDurations(i)))
		
		val horizon = oDurations.map(_*coefficient.max).sum
	
		// Modeling	
		// -----------------------------------------------------------------------
  	   	
  	   	val cp = CPSolver()

		// Matrix of cumulative activities (each line represents a job)
		val activities = Array.tabulate(nTasks)(i => {
			
  	   		val dur     = CPVarInt(cp, durations(i).min to durations(i).max)
  	   		val start   = CPVarInt(cp, 0 to horizon - dur.getMin)
  	   		val machine = CPVarInt(cp, machines(i))
  	   		
  	   		CumulativeActivity(start,dur, machine, 1)
  	   	}) 	   
  	   	
  	   	// The make span to minimize
  	   	val makespan = maximum(Tasks)(i => activities(i).end)
  	   	
  	   	// Visualization  
  	   	// -----------------------------------------------------------------------
  	   	 	
  	   	val frame = new VisualFrame("Cumulative Job-Shop Problem", 1, 1)
		
		val cols = VisualUtil.getRandomColorArray(nTasks)
		val visualActivities = activities.map(a => VisualActivity(a))
		
		// Gantt Chart
		val gantt = new VisualGanttChart(visualActivities, cols, visualActivities(_).machine)
		frame.createFrame("Activities").add(gantt)
  	   	
  	   	// Constraints and Solving
		// -----------------------------------------------------------------------
		
		//cp.failLimit(20000)
		
  	   	cp.minimize(makespan) subjectTo {
			
			// Precedences
			cp.add(activities(0).end <= activities(1).start)
			cp.add(activities(0).end <= activities(2).start)
			cp.add(activities(0).end <= activities(3).start)
			cp.add(activities(1).end <= activities(4).start)
			cp.add(activities(2).end <= activities(5).start)	
			cp.add(activities(3).end <= activities(4).start)
			
			// Durations
			for (i <- Tasks)
				cp.add(element(durations(i), activities(i).machine, activities(i).dur))
			
			// Cumulative constraints
			for (i <- Machines)
				cp.add(new MaxSweepCumulative(cp, activities, 1, i))

		} exploration {
			
			// Efficient but not complete search strategy
			cp.setTimes(activities)
			cp.binary(activities.map(_.dur))
			
			// Updates the visual components
			gantt.update(1, 20)
		}    
		
		cp.printStats() 
	}
}
