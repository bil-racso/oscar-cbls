package oscar.cp.scheduling

import oscar.cp.modeling.CPScheduler
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