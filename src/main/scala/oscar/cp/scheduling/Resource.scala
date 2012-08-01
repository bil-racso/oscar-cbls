package oscar.cp.scheduling

import scala.collection.mutable.Set
import oscar.cp.modeling.CPScheduler

import oscar.cp.scheduling._

abstract class Resource(scheduler : CPScheduler, private var name : String) {
	
	def this(scheduler : CPScheduler) = this(scheduler, null)
	
	// Link the resource to the scheduler and get an id
	val id = scheduler.addResource(this)
	
	// Generic name
	if (name == null) name = "Resource " + id
	
	override def toString = name
	
	def setup()
}