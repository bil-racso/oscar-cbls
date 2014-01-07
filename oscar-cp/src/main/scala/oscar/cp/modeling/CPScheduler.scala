/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/


package oscar.cp.modeling

import oscar.cp.scheduling._
import oscar.algo.reversible._
import oscar.cp.core.CPVarInt
import scala.util.continuations._
import scala.collection.JavaConverters._
import scala.collection.mutable.Map
import oscar.cp.core.NoSolutionException

class CPScheduler(var horizon : Int) extends CPSolver {

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
			case ex : NoSolutionException => println("No Solution, inconsistent model (resource constraints)")
		}
	}

	override def subjectTo(constraintsBlock : => Unit) : CPSolver = {

		super.subjectTo {
		  // do not swap these two lines
		  constraintsBlock
		  addResourceConstraints()
		}
		
		this
	}

}

object CPScheduler {
	def apply(horizon : Int) = new CPScheduler(horizon)
}
