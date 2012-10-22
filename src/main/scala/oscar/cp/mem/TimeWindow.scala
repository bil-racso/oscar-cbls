package oscar.cp.mem

import oscar.cp.core._
import scala.math.max

class TimeWindow(cp: Store, c : Int, pred: Array[CPVarInt], succ: Array[CPVarInt], arrival: Array[CPVarInt], dist: Array[Array[Int]], service: Array[Int]) extends Constraint(cp, "TimeWindow") {

	val nCustomers = pred.size
	val Customers = 0 until nCustomers

	override def setup(l: CPPropagStrength): CPOutcome = {

		if (predPropagate(c) == CPOutcome.Failure)
			CPOutcome.Failure
		if (succPropagate(c) == CPOutcome.Failure)
			CPOutcome.Failure

		if (!pred(c).isBound)
			pred(c).callPropagateWhenDomainChanges(this)

		if (!arrival(c).isBound) {
			arrival(c).callUpdateMaxIdxWhenMaxChanges(this, c)
			arrival(c).callUpdateMinIdxWhenMinChanges(this, c)
		}

		CPOutcome.Suspend
	}

	override def updateMinIdx(cvar: CPVarInt, c: Int, v: Int): CPOutcome = {

		for (j <- succ(c)) {

			if (arrival(c).min + service(c) + dist(c)(j) > arrival(j).max) {

				if (succ(c).removeValue(j) == CPOutcome.Failure)
					CPOutcome.Failure
			}
		}

		CPOutcome.Suspend
	}

	override def updateMaxIdx(cvar: CPVarInt, c: Int, v: Int): CPOutcome = {

		for (j <- pred(c)) {

			if (arrival(j).min + service(j) + dist(j)(c) > arrival(c).max) {

				if (pred(c).removeValue(j) == CPOutcome.Failure)
					CPOutcome.Failure
			}
		}

		CPOutcome.Suspend
	}

	override def propagate(): CPOutcome = {

		if (predPropagate(c) == CPOutcome.Failure)
			CPOutcome.Failure

		if (succPropagate(c) == CPOutcome.Failure)
			CPOutcome.Failure

		CPOutcome.Suspend
	}

	def predPropagate(c: Int): CPOutcome = {

		val timesMin = Array.fill(nCustomers)(Int.MaxValue)
		val timesMax = Array.fill(nCustomers)(Int.MaxValue)

		var minV = Int.MaxValue
		var maxV = Int.MinValue

		for (j <- pred(c)) {

			val tauMin = arrival(j).min + service(j) + dist(j)(c)
			val tauMax = arrival(j).max + service(j) + dist(j)(c)

			if (tauMin < minV)
				minV = tauMin

			if (tauMax > maxV)
				maxV = tauMax
		}

		if (arrival(c).updateMin(minV) == CPOutcome.Failure)
			CPOutcome.Failure

		if (arrival(c).updateMax(maxV) == CPOutcome.Failure)
			CPOutcome.Failure

		CPOutcome.Suspend
	}

	def succPropagate(c: Int): CPOutcome = {

		val timesMin = Array.fill(nCustomers)(Int.MaxValue)
		val timesMax = Array.fill(nCustomers)(Int.MaxValue)

		var minV = Int.MaxValue
		var maxV = Int.MinValue

		for (j <- succ(c)) {

			val tauMin = arrival(j).min - service(c) - dist(c)(j)
			val tauMax = arrival(j).max - service(c) - dist(c)(j)

			if (tauMin < minV)
				minV = tauMin

			if (tauMax > maxV)
				maxV = tauMax
		}

		if (arrival(c).updateMin(minV) == CPOutcome.Failure)
			CPOutcome.Failure

		if (arrival(c).updateMax(maxV) == CPOutcome.Failure)
			CPOutcome.Failure

		CPOutcome.Suspend
	}
}

