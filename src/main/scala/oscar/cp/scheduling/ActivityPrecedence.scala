package oscar.cp.scheduling

import oscar.cp.core.CPVarInt

class ActivityPrecedence(v : CPVarInt, d : Int, exactly : Boolean) {
	
	private val cp = v.store

	def beforeEndOf(act : Activity)   = if (exactly) cp.add(v + d == act.end) else cp.add(v + d <= act.end)
	def beforeStartOf(act : Activity) = if (exactly) cp.add(v + d == act.start) else cp.add(v + d <= act.start)
}

object ActivityPrecedence {

	def apply(v : CPVarInt, i : Int, b : Boolean = false) = new ActivityPrecedence(v, i, b)
}
