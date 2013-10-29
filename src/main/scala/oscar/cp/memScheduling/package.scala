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
package oscar.cp

import oscar.cp.core.CPVarInt
import oscar.cp.modeling.CPScheduler
import oscar.cp.scheduling._
import scala.collection.TraversableLike

/**
 * @author Renaud Hartert : ren.hartert@gmail.com
 */
package object memScheduling {
	
	// ImplicitVarInt
	implicit def var2ImplicitVarInt(v : CPVarInt)     = VarImplicitVarInt(v)
	implicit def array2ImplicitVarInt(a : Array[Int]) = ArrayImplicitVarInt(a)
	implicit def range2ImplicitVarInt(r : Range)      = RangeImplicitVarInt(r)
	implicit def int2ImplicitVarInt(i : Int)          = IntImplicitVarInt(i)
	
	// ImplicitVarInt over arrays
	implicit def intArray2ImplicitVarIntArray(a: Array[Int]): Array[ImplicitVarInt]  = a.map(IntImplicitVarInt(_))
	
	// Adding functions over Activity arrays
	implicit def arrayActivityOps(activities: Array[Activity]) = new {
	  def needs(requirements: Array[Int]): ActivitiesRequirements = {
	    ActivitiesRequirements(activities, requirements)
	  }
	}
}
