package oscar.cp.constraints
import oscar.cp.core._
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength

class CPObjectiveMinimize(objVars: CPVarInt*) extends CPObjective(objVars: _*) {

	for (i <- 0 until bounds.size) {
		bounds(i) = Int.MaxValue
		bestObjs(i) = Int.MaxValue
	}

	private val lowerBounds = Array.tabulate(objVars.size)(i => objVars(i).min)

	override def tighten() = {
		if (!currentObjective.isBound) {
			throw new RuntimeException("objective not bound:" + currentObjective)
		}
		println("objective " + currentObjectiveIdx + " tightened to " + (bound).min(currentObjective.value) + " lb:" + lowerBounds(currentObjectiveIdx))
		for (i <- 0 until objVars.size) {
			bestObjs(i) = objVars(i).value
			bounds(i) = bestObjs(i)
			tightenedOnce(i) = true
		}
		bestObj = currentObjective.value
		bound = bestObj - 1
		tightenedOnce(currentObjectiveIdx) = true
	}

	override def reTighten() = {
		bound = bestObj - 1
	}

	override def relax() = {
		bound = Int.MaxValue
		bestObj = Int.MaxValue
	}

	override def isOptimum() = {
		(0 until bounds.size).forall(i => bounds(i) < lowerBounds(i))
	}

	override def propagate(): CPOutcome = {
		for (i <- 0 until objVars.size) {
			if (objVars(i).updateMax(bounds(i)) == CPOutcome.Failure) {
				return CPOutcome.Failure
			}
		}
		CPOutcome.Suspend;
	}

	override def restoreBest() = {
		bound = bestObj
	}

}
