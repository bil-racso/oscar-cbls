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

package oscar.cp.modeling

import oscar.cp.scheduling._
import oscar.reversible.ReversibleBool
import oscar.reversible.ReversibleInt

import scala.util.continuations._
import scala.collection.JavaConverters._
import scala.collection.mutable.Set

class CPScheduler(val horizon : Int) extends CPSolver {

	private var nResource = 0
	private val resources : Set[Resource] = Set()

	def addResource(resource : Resource) : Int = {

		resources.add(resource)
		nResource += 1

		return nResource
	}
	
	def addResourceConstraints() = {

		try {
			for (r <- resources) r.setup
		} catch {
			case ex : NoSol => println("No Solution, inconsistent model (resource constraints)")
		}
	}

	override def subjectTo(constraintsBlock : => Unit) : CPSolver = {
		
		try {
			constraintsBlock
		} catch {
			case ex : NoSol => println("No Solution, inconsistent model")
		}
		
		addResourceConstraints()

		this
	}

	def setTimesSearch(activities : Array[Activity]) : Unit @suspendable = {

		// Non fixed activities
		val selectable = Array.tabulate(activities.size) {
			i =>
				if (activities(i).start.isBound)
					new ReversibleBool(this, false)
				else
					new ReversibleBool(this, true)
		}

		val oldEST = Array.fill(activities.size)(new ReversibleInt(this, -1))

			def updateSelectable() = {

				for (i <- 0 until activities.size) {
					if (activities(i).start.isBound) {
						selectable(i).value = false

					} else if (oldEST(i).value != activities(i).est) {
						selectable(i).value = true
					}
				}
			}

			def selectableIndices() = (0 until activities.size).filter(i => selectable(i).value)

			def allStartBounds() = activities.forall(i => i.start.isBound)

		while (!allStartBounds()) {

			// Get the smallest EST
			val (est, ect) = selectableIndices().map(i => (activities(i).est, activities(i).ect)).min

			// Select the activity with the smallest EST, ECT as tie breaker
			val x = selectableIndices().filter(i => activities(i).est == est && activities(i).ect == ect).first

			branch {

				post(activities(x).start == est)
				oldEST(x).value = -1
				updateSelectable()

				if (selectableIndices().isEmpty && !allStartBounds()) fail()
			} {

				selectable(x).value = false
				oldEST(x).value = est
				updateSelectable()

				if (selectableIndices().isEmpty && !allStartBounds()) fail()
			}
		}
	}
}