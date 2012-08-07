package oscar.cp.scheduling

import oscar.cp.modeling.CPScheduler
import oscar.cp.core.CPVarInt

sealed trait Capacity {
	def variable(scheduler : CPScheduler) : CPVarInt
	def opposite(scheduler : CPScheduler) : CPVarInt
}
case class VariableCapacity(variable : CPVarInt) extends Capacity {
	override def variable(scheduler : CPScheduler) : CPVarInt = variable
	override def opposite(scheduler : CPScheduler) : CPVarInt = -variable(scheduler)
}
case class RangeCapacity(r : Range) extends Capacity {
	override def variable(scheduler : CPScheduler) = CPVarInt(scheduler, r)
	override def opposite(scheduler : CPScheduler) = CPVarInt(scheduler, -r.max to -r.min)
}
case class IntCapacity(i : Int) extends Capacity {
	override def variable(scheduler : CPScheduler) = CPVarInt(scheduler, i)
	override def opposite(scheduler : CPScheduler) = CPVarInt(scheduler, -i)
}