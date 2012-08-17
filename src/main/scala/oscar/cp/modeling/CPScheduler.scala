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

package oscar.cp.modeling

import oscar.cp.scheduling._
import oscar.reversible.ReversibleBool
import oscar.reversible.ReversibleInt
import oscar.cp.core.CPVarInt

import scala.util.continuations._
import scala.collection.JavaConverters._
import scala.collection.mutable.Map

class CPScheduler(val horizon : Int) extends CPSolver {

	private var nResource = 0
	private var nActivity = 0
	private val resourcesMap  : Map[Int, Resource] = Map()
	private val activitiesMap : Map[Int, Activity] = Map()
	
	def resource(i : Int) = resourcesMap(i)
	def activity(i : Int) = activitiesMap(i)
	
	def makespan = maximum(activities.map(_.end))
	
	def addResource(resource : Resource) : Int = {

		val temp = nResource
		resourcesMap += temp -> resource
		nResource += 1

		return temp
	}
	
	def addActivity(activity : Activity) : Int = {

		val temp = nActivity
		activitiesMap += temp -> activity
		nActivity += 1

		return temp
	}
	
	def activities = activitiesMap.values.toArray
	def resources  = resourcesMap.values.toArray

	def addResourceConstraints() = {

		try {
			resourcesMap foreach (r => r._2.setup)
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

	/**
	 * Binary First Fail on the decision variables vars
	 */
	def binaryFirstFail(vars : Array[Activity]) : Unit @suspendable = {
		binaryFirstFail(vars.map(_.start))
	}

	/**
	 * Binary search on the decision variables vars with custom variable/value heuristic
	 */
	def binary(vars : Array[Activity]) : Unit @suspendable = {
		binary(vars.map(_.start))
	}

	
	def setTimes(activities : Array[_ <: Activity]) : Unit @suspendable = {

		val n = activities.size
		val Activities = 0 until n
		
		val selectable = Array.fill(n)(new ReversibleBool(this, true))
		// non fixed activities (by setTimes)
		val bound = Array.fill(n)(new ReversibleBool(this, false))

		val oldEST = Array.fill(n)(new ReversibleInt(this, -1))
	
		// update the new ones becoming available because est has moved
		def updateSelectable() = (Activities).filter(i => oldEST(i).value < activities(i).est).foreach(selectable(_).value = true)
		def selectableIndices() = (Activities).filter(i => selectable(i).value && !bound(i).value)
		def allStartBounds() = bound.forall(i => i.value)
		
		def updateAndCheck() = {
			updateSelectable()
			if (selectableIndices().isEmpty && !allStartBounds()) this.fail()
		}
		
		while (!allStartBounds()) {
			
			updateSelectable()
			val (est, ect) = selectableIndices().map(i => (activities(i).est, activities(i).ect)).min
			
			// Select the activity with the smallest EST, ECT as tie breaker
			val x = selectableIndices().filter(i => activities(i).est == est && activities(i).ect == ect).first
			
			branch {
				this.post(activities(x).start == est)
				bound(x).value = true
				if (!this.isFailed) updateAndCheck()
			} {
				selectable(x).value = false
				oldEST(x).value = est
				updateAndCheck()
			}
		}
	}
}

object CPScheduler {

	def apply(horizon : Int) = new CPScheduler(horizon)
}