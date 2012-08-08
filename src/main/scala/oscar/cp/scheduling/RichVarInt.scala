package oscar.cp.scheduling

import oscar.cp.modeling.CPScheduler
import oscar.cp.core.CPVarInt

sealed trait RichVarInt {
	
	def variable(scheduler : CPScheduler) : CPVarInt
	def opposite(scheduler : CPScheduler) : CPVarInt
	
	def isPositive : Boolean
	
	def minVal : Int
	def maxVal : Int
}

case class VarRichVarInt(v : CPVarInt) extends RichVarInt {
	
	override def variable(scheduler : CPScheduler) : CPVarInt = v
	override def opposite(scheduler : CPScheduler) : CPVarInt = -v
	
	override def isPositive = v.min >= 0
	
	override def minVal = v.min
	override def maxVal = v.max
}

case class ArrayRichVarInt(a : Array[Int]) extends RichVarInt {

	override def variable(scheduler : CPScheduler) = CPVarInt(scheduler, a)
	override def opposite(scheduler : CPScheduler) = CPVarInt(scheduler, a.map(-_))
	
	override def isPositive = a.min >= 0
	
	override def minVal = a.min
	override def maxVal = a.max
}

case class RangeRichVarInt(r : Range) extends RichVarInt {
	
	override def variable(scheduler : CPScheduler) = CPVarInt(scheduler, r)
	override def opposite(scheduler : CPScheduler) = CPVarInt(scheduler, -r.max to -r.min)
	
	override def isPositive = r.min >= 0
	
	override def minVal = r.min
	override def maxVal = r.max
}

case class IntRichVarInt(i : Int) extends RichVarInt {
	
	override def variable(scheduler : CPScheduler) = CPVarInt(scheduler, i)
	override def opposite(scheduler : CPScheduler) = CPVarInt(scheduler, -i)
	
	override def isPositive = i >= 0
	
	override def minVal = i
	override def maxVal = i
}