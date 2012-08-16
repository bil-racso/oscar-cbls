package oscar.cp.scheduling

import oscar.cp.modeling.CPScheduler
import oscar.cp.core.CPVarInt

sealed trait ImplicitVarInt {
	
	def variable(scheduler : CPScheduler) : CPVarInt
	def opposite(scheduler : CPScheduler) : CPVarInt
	
	def isPositive : Boolean
	
	def minVal : Int
	def maxVal : Int
}

case class VarImplicitVarInt(v : CPVarInt) extends ImplicitVarInt {
	
	override def variable(scheduler : CPScheduler) : CPVarInt = v
	override def opposite(scheduler : CPScheduler) : CPVarInt = -v
	
	override def isPositive = v.min >= 0
	
	override def minVal = v.min
	override def maxVal = v.max
}

case class ArrayImplicitVarInt(a : Array[Int]) extends ImplicitVarInt {

	override def variable(scheduler : CPScheduler) = CPVarInt(scheduler, a)
	override def opposite(scheduler : CPScheduler) = CPVarInt(scheduler, a.map(-_))
	
	override def isPositive = a.min >= 0
	
	override def minVal = a.min
	override def maxVal = a.max
}

case class RangeImplicitVarInt(r : Range) extends ImplicitVarInt {
	
	override def variable(scheduler : CPScheduler) = CPVarInt(scheduler, r)
	override def opposite(scheduler : CPScheduler) = CPVarInt(scheduler, -r.max to -r.min)
	
	override def isPositive = r.min >= 0
	
	override def minVal = r.min
	override def maxVal = r.max
}

case class IntImplicitVarInt(i : Int) extends ImplicitVarInt {
	
	override def variable(scheduler : CPScheduler) = CPVarInt(scheduler, i)
	override def opposite(scheduler : CPScheduler) = CPVarInt(scheduler, -i)
	
	override def isPositive = i >= 0
	
	override def minVal = i
	override def maxVal = i
}