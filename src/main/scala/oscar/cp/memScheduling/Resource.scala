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
package oscar.cp.memScheduling

import scala.collection.mutable.Set

abstract class Resource(val scheduler : CPScheduler, n: String = null) {
	
	// Link the resource to the scheduler and get an id
	val id = scheduler.addResource(this)
	
	val name =  Option(n) getOrElse ("Resource " + id)
	
	override def toString = name
	
	def setup()
}
