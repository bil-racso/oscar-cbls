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
package oscar.cp.scheduling;

import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;
import oscar.cp.constraints.LeEq
import oscar.cp.modeling.CPScheduler

class Activity(val scheduler : CPScheduler, durVar: CPVarInt) {
    
	private val startVar : CPVarInt = CPVarInt(scheduler, 0 to scheduler.horizon - durVar.min)
    private val endVar   : CPVarInt = CPVarInt(scheduler, durVar.min to scheduler.horizon) 
    scheduler.add(startVar + durVar == endVar) // Linking the variables

    def start = startVar
    def end = endVar
    def dur = durVar
    
	def this(scheduler : CPScheduler, dur: Int) = this(scheduler, CPVarInt(scheduler,dur,dur))

	/**
	 * earliest starting time
	 */
	def est = start.min
	
	/**
	 * latest starting time
	 */
	def lst = start.max
	
	/**
	 * earliest completion time assuming the smallest duration
	 */
	def ect = end.min
	
	/**
	 * latest completion time assuming the smallest duration
	 */
	def lct = end.max
	
	/**
	 * current minimal duration of this activity
	 */
	def minDuration = dur.min

	/**
	 * current maximal duration of this activity
	 */
	def maxDuration = dur.max
	
	def adjustStart(v : Int) = start.updateMin(v)	
	
	def <<(act : Activity) : LeEq = this.end <= act.start
	
	def >>(act : Activity) : LeEq = act.end <= this.start
	
	def precedes(act : Activity) : LeEq = this << act
	
	def follows(act : Activity) : LeEq = act << this
	
	def needs(resource : CumulativeResource, capacity : Int) {
    	assert(capacity >= 0)
		resource.addActivity(this, capacity)
    }
	
	def needsForever(resource : CumulativeResource, capacity : Int, atEnd : Boolean = true) {
    	assert(capacity >= 0)
		resource.addProdConsActivity(this, capacity, atEnd)
    }
	
	def suppliesForever(resource : CumulativeResource, capacity : Int, atEnd : Boolean = true) {
    	assert(capacity >= 0)
		resource.addProdConsActivity(this, -capacity, atEnd)
    }
	
	def needs(resource : CumulativeResource, capacity : Range) {
    	assert(capacity.min >= 0)
		resource.addActivity(this, capacity)
	}
	
	def needs(resource : CumulativeResourceSet, resources : Array[Int], capacity : Range) {
		assert(capacity.min >= 0)
		resource.addActivity(this, resources, capacity)	
	}
	
	def needs(resource : CumulativeResourceSet, resources : Array[Int], capacity : Int) {
		assert(capacity >= 0)
		resource.addActivity(this, resources, capacity)	
	}
	
	def supplies(resource : CumulativeResourceSet, resources : Array[Int], capacity : Range) {
		assert(capacity.min >= 0)
		resource.addActivity(this, resources, -capacity.max to -capacity.min)	
	}
	
	def supplies(resource : CumulativeResourceSet, resources : Array[Int], capacity : Int) {
		assert(capacity >= 0)
		resource.addActivity(this, resources, -capacity)	
	}
	
	def supplies(resource : CumulativeResource, capacity : Int) {
    	assert(capacity >= 0)
    	resource.addActivity(this, -capacity)
    }
	
	def supplies(resource : CumulativeResource, capacity : Range) {
    	assert(capacity.min >= 0)
		resource.addActivity(this, -capacity.max to -capacity.min)
	}
	
	def needs(resource : UnitResource) {
    	resource.addActivity(this)
    }
	
	def store = scheduler
	
	override def toString = "dur: "+dur+ " in ["+est+","+lct+"["
}

object Activity {
	
	def apply(scheduler : CPScheduler, dur : Int) = new Activity(scheduler, dur)
	
	def apply(scheduler : CPScheduler, dur : Range) = new Activity(scheduler, CPVarInt(scheduler, dur))

	def apply(scheduler : CPScheduler, durVar : CPVarInt) = new Activity(scheduler, durVar)
}



class MirrorActivity(val act: Activity)  extends Activity(act.scheduler, act.dur) {

	override def start: CPVarInt = throw new UninitializedFieldError("not available") 
	
	override def end: CPVarInt = throw new UninitializedFieldError("not available") 
	
	/**
	 * earliest starting time
	 */
	override def est = - act.lct;
	
	/**
	 * latest starting time
	 */
	override def lst = - act.ect;
	
	/**
	 * earliest completion time assuming the smallest duration
	 */
	override def ect = - act.lst

	/**
	 * latest completion time assuming the smallest duration
	 */
	override def lct = - act.est
	
	override def adjustStart(v : Int) = end.updateMax(-v)

	override def toString() = "mirror of activity:"+act;
	
	override def <<(act : Activity) = throw new UninitializedFieldError("not available") 
	
	override def >>(act : Activity) = throw new UninitializedFieldError("not available") 
}
