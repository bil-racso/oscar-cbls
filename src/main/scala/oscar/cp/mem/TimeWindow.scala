package oscar.cp.mem

import oscar.cp.core._
import scala.math.max

class TimeWindow(cp: Store, c: Int, pred: Array[CPVarInt], succ: Array[CPVarInt], arrival: Array[CPVarInt], dist: Array[Array[Int]], service: Array[Int]) extends Constraint(cp, "TimeWindow") {

	override def setup(l: CPPropagStrength): CPOutcome = {

		val oc = propagate

		if (oc != CPOutcome.Failure) {

			if (!pred(c).isBound)
				pred(c).callPropagateWhenDomainChanges(this)

			if (!arrival(c).isBound) {
				arrival(c).callUpdateMaxWhenMaxChanges(this)
				arrival(c).callUpdateMinWhenMinChanges(this)
			}
		}

		return oc
	}

	override def updateMin(cvar: CPVarInt, v: Int): CPOutcome = {

		for (j <- succ(c)) {

			if (arrival(c).min + service(c) + dist(c)(j) > arrival(j).max) {

				if (succ(c).removeValue(j) == CPOutcome.Failure)
					return CPOutcome.Failure
			}
		}

		return CPOutcome.Suspend
	}

	override def updateMax(cvar: CPVarInt, v: Int): CPOutcome = {

		for (j <- pred(c)) {

			if (arrival(j).min + service(j) + dist(j)(c) > arrival(c).max) {

				if (pred(c).removeValue(j) == CPOutcome.Failure)
					return CPOutcome.Failure
			}
		}

		return CPOutcome.Suspend
	}

	override def propagate(): CPOutcome = {

		if (predPropagate(c) == CPOutcome.Failure)
			return CPOutcome.Failure

		if (succPropagate(c) == CPOutcome.Failure)
			return CPOutcome.Failure

		return CPOutcome.Suspend
	}

	def predPropagate(c: Int): CPOutcome = {

		if (pred(c).isBound)
			return CPOutcome.Suspend

		else {

			var minV = Int.MaxValue

			for (j <- pred(c).min to pred(c).max; if (pred(c).hasValue(j))) {
				val tauMin = arrival(j).min + service(j) + dist(j)(c)
				if (tauMin < minV)
					minV = tauMin
			}

			if (arrival(c).updateMin(minV) == CPOutcome.Failure)
				return CPOutcome.Failure

			else return CPOutcome.Suspend
		}
	}

	def succPropagate(c: Int): CPOutcome = {

		if (pred(c).isBound)
			return CPOutcome.Suspend

		else {
			
			var maxV = Int.MinValue

			for (j <- succ(c).min to succ(c).max; if (succ(c).hasValue(j))) {
				val tauMax = arrival(j).max - service(c) - dist(c)(j)
				if (tauMax > maxV)
					maxV = tauMax
			}

			if (arrival(c).updateMax(maxV) == CPOutcome.Failure)
				return CPOutcome.Failure

			else return CPOutcome.Suspend
		}
	}
}

