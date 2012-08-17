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

package oscar.cp.scheduling

import scala.collection.mutable.Set
import scala.collection.mutable.Map
import oscar.cp.modeling._
import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.core.CPVarInt
import oscar.reversible.ReversibleBool
import java.security.InvalidParameterException
import scala.util.continuations._
import oscar.reversible.ReversibleBool


/**
 * authors: Renaud Hartert and Pierre Schaus
 */
class UnitResource(scheduler : CPScheduler) extends Resource(scheduler) {

	protected val activitiesSet : Set[Activity] = Set()

	def activities = activitiesSet.toArray

	def addActivity(activity : Activity) {
		activitiesSet.add(activity)
	}

	override def setup() {
		scheduler.add(unaryResource(activities))
		scheduler.add(cumulative(activities.map(act => CumulativeActivity(act,id,1)), id, max = 1))
	}
	
	/**
	 * efficient search to impose a total order of the activities requiring this resource
	 */
	def rank(): Unit @suspendable =  {
	    val activs = activities
	    val ranked = Array.fill(activs.size)(new ReversibleBool(scheduler,false))
	    def isRanked = ranked.forall(_.value == true)
	    def rankFirst(j: Int) = {
		 for (i <- 0 until activs.size) {
			if (i!= j /*&& required(i).isTrue()*/ && !ranked(i).value) {
				scheduler.post(activities(j).end <= activities(i).start)
			}
		 }
		 ranked(j).value = true
	    }
	    
	  	while (!isRanked) {
	         val toRank = (0 until activs.size).filter(!ranked(_).value).sortBy(i => (activs(i).est,activs(i).lst))
	         // try all not yet ranked activities as the first one to rank
	         scheduler.branchAll(toRank)(i => rankFirst(i))
	    }
	}
	
	

	/**
	 * a number between 0/1 representing the business of the resource over it's horizon
	 * close to 1 means that almost at any point there is an activity executing, close to 0 is the opposite
	 */	
	def criticality = {
	  val activs = activities
	  val max = activs.map(_.lct).max
	  val min = activs.map(_.est).min
	  activities.map(_.minDuration).sum.toDouble / (max-min)
	}
}

object UnitResource {

	def apply(scheduler : CPScheduler) = new UnitResource(scheduler)
}