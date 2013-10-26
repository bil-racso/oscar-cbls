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

import oscar.cp.core.CPVarInt

abstract class ImplicitVarInt {
	
	protected var opp : Boolean = false
	
	def toCPVarInt(scheduler : CPScheduler) : CPVarInt
	
	def unary_-() = this.opposite()
	def opposite() : ImplicitVarInt = {	
		opp = !opp
		return this
	}
	
	def isPositive : Boolean
	
	def minVal : Int
	def maxVal : Int
}

case class VarImplicitVarInt(v : CPVarInt) extends ImplicitVarInt {
	
	override def toCPVarInt(scheduler : CPScheduler) = if (!opp) v else -v
	
	override def isPositive = v.min >= 0
	
	override def minVal = v.min
	override def maxVal = v.max
}

case class ArrayImplicitVarInt(a : Array[Int]) extends ImplicitVarInt {

	override def toCPVarInt(scheduler : CPScheduler) = if (!opp) CPVarInt(scheduler, a) else CPVarInt(scheduler, a.map(-_))
	
	override def isPositive = a.min >= 0
	
	override def minVal = a.min
	override def maxVal = a.max
}

case class RangeImplicitVarInt(r : Range) extends ImplicitVarInt {
	
	override def toCPVarInt(scheduler : CPScheduler) = if (!opp) CPVarInt(scheduler, r) else CPVarInt(scheduler, -r.max to -r.min)
	
	override def isPositive = r.min >= 0
	
	override def minVal = r.min
	override def maxVal = r.max
}

case class IntImplicitVarInt(i : Int) extends ImplicitVarInt {
	
	override def toCPVarInt(scheduler : CPScheduler) = if (!opp) CPVarInt(scheduler, i) else CPVarInt(scheduler, -i)
	
	override def isPositive = i >= 0
	
	override def minVal = i
	override def maxVal = i
}
