package oscar.cp.scheduling

import scala.collection.mutable.Set
import oscar.cp.modeling.CPScheduler

import oscar.cp.scheduling._

abstract class Resource(val scheduler : CPScheduler, n: String = null) {
	
	// Link the resource to the scheduler and get an id
	val id = scheduler.addResource(this)
	
	val name =  Option(n) getOrElse ("Resource " + id)
	
	override def toString = name
	
	def setup()
}