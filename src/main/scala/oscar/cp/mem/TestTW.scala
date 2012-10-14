package oscar.cp.mem

import oscar.cp.core._
import oscar.cp.modeling._

object TestTW extends App {
	
	val cp = CPSolver()
	
	val distance  = Array(20, 5)	
	val departure = Array(CPVarInt(cp, 10 to 15), CPVarInt(cp, 20 to 25))
	
	val arrival = CPVarInt(cp, 0 to 50)
	val prev = CPVarInt(cp, 0 to 1)
	
	//cp.add(arrival == departure(prev) + distance(prev))
	cp.add(new TimeWindow(cp, prev, arrival, departure, distance))
	
	println(arrival)
}