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

import oscar.cp.constraints.NaiveMultiCumulative
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.cp.scheduling._
import oscar.visual._
import scala.io.Source
import oscar.cp.constraints.NaiveMultiCumulative


object CumulativeJobShop extends CPModel {
  
	def main(args: Array[String]) {
	  
		// Read the data
		var lines = Source.fromFile("data/cJobShop.txt").getLines.toList
		
		// Parsing
		val nJobs    = lines.head.trim().split(" ")(0).toInt // number of jobs
		val nTasks   = lines.head.trim().split(" ")(1).toInt // number of machines and tasks per job
		val capacity = lines.head.trim().split(" ")(2).toInt // capacity
		
		println("#Jobs     " + nJobs)
		println("#Tasks    " + nTasks)
		println("#Machines " + nTasks)
		println("#Capacity " + capacity)
		
		lines = lines.drop(1)
		
		val machines   : Array[Array[Int]] = Array.fill(nJobs, nTasks)(0)
		val durations  : Array[Array[Int]] = Array.fill(nJobs, nTasks)(0)
		val capacities : Array[Int] = Array.fill(nTasks)(capacity);
		
		for (i <- 0 until nJobs) {
			val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
			
			println("job "+ (i+1) +"\t" + l.mkString(" "))
			
			machines(i)  = Array.tabulate(nTasks)(j => l(2*j))
			durations(i) = Array.tabulate(nTasks)(j => l(2*j+1))
	  	    lines = lines.drop(1)
		}
		
		val horizon = durations.flatten.sum*3/(2*nJobs)
		
		// Visualization
		val frame = new VisualFrame("Cumulative Job-Shop Problem")						  
		val profile = new VisualProfile(false)
		val inf = frame.createFrame("Ressource Consumption")
		inf.add(profile)
		frame.pack

		// Modeling
		val cp = CPSolver()

		// A cumulative JobShop activity
		class CumJobShopAct(val act : CumulativeActivity, val job: Int)
	   
		val activities = Array.tabulate(nJobs, nTasks)((i,j) => {
			
  	   		val dur      = CPVarInt(cp, durations(i)(j))
  	   		val start    = CPVarInt(cp,0 to horizon - dur.getMin())
  	   		
  	   		new CumJobShopAct(CumulativeActivity(start,dur, machines(i)(j), 1), i) 
  	   	}) 	   
  	   	
  	   	val act_machines  = activities.flatten.map(_.act.getMachine)   
  	   	val act_starts    = activities.flatten.map(_.act.getStart())  
  	   	val act_durations = activities.flatten.map(_.act.getDur())
  	   	val act_ends      = activities.flatten.map(_.act.getEnd())
  	   	val act_resources = activities.flatten.map(_.act.getResource)  
  	   	
  	   	val makespan = maximum(0 until nJobs)(i => activities(i)(nTasks-1).act.getEnd)
             
  	   	// Solving
  	   	cp.minimize(makespan) subjectTo {

			// add the precedence constraints inside a job
			for (i <- 0 until nJobs; j <- 0 until nTasks-1) {
				cp.add(activities(i)(j).act.getEnd() <= activities(i)(j+1).act.getStart())
			}
			
			for (i <- 0 until act_starts.size) 
				cp.add(act_starts(i) + act_durations(i) == act_ends(i))
			
			// add the unary resources
	  	   NaiveMultiCumulative.multiCumulative(cp, act_machines, act_starts, act_durations, act_resources, capacities)

	   } exploration {
	       
		   cp.binaryFirstFail(act_starts)
		   
		   //Update visualization
		   
	   }
       cp.printStats()
	}
}
