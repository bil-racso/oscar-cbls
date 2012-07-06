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

package oscar.cp.constraints

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.scheduling.CumulativeActivity

/**This is a brute-force decomposition of the global constraint describe in "A New Multi-Resource 
 * Cumulatives Constraint with Negative Heights" by Nicolas Beldiceanu and Mats Carlsson.
 * 
 * It was build as a support during during the implementation of the global constraint from the 
 * same reference.
 * 
 * @author Renaud Hartert - ren.hartert@gmail.com
 */

object NaiveMultiCumulative extends CPModel {
  
	// m : machine of each task
	// s : start of each task
	// d : duration of each task
	// r : resource consumption of each task
	// c : capacity of each machine
	  
	// Note : this decomposition does not enforce e = s+d !
	
	def multiCumulative(cp: CPSolver, activities : Array[CumulativeActivity], c : Array[Int]) {
		
		multiCumulative(cp, 
						activities.map(_.mach), 
						activities.map(_.getStart), 
						activities.map(_.getDur), 
						activities.map(_.getResource),
						c)
	}
  
	def multiCumulative(cp: CPSolver,
						m: Array[CPVarInt],
						s: Array[CPVarInt],
						d: Array[CPVarInt],
						r: Array[CPVarInt],
						c: Array[Int]) {
		
		val Tasks    = 0 until s.size
		val Machines = 0 until c.size
    
		// Keep tasks with positive duration
		val tasks = (Tasks).filter(i => d(i).getMax > 0)

		// Boundaries of the total processing time
		val t_min = tasks.map(i => s(i).getMin).min
		val d_max = tasks.map(i => d(i).getMax).max
		val t_max = tasks.map(i => s(i).getMax + d_max).max
            
		// For all the instant t
		for (t <- t_min to t_max) {
			
			println("Instant " + t + " of horizon : " + t_min + ".." + t_max)
		    val tasks = (Tasks).filter(i => d(i).getMax > 0 && s(i).getMin() <= t && s(i).getMax()+d(i).getMax() > t)
		    
		    if (!tasks.isEmpty) {
				// For all machines
				for (machine <- Machines) {
	
					// Get all tasks on this machine
	 				def overlap(i: Int) = (m(i) === machine) && (s(i) <== t) && (s(i)+d(i) >>= t)
					// The consumption of all those tasks must be leq than the capacity of the machine
	 				cp.add(sum(tasks)(i => overlap(i)*r(i)) <== c(machine))
				}
		    }
		}    
	}
}