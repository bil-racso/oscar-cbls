/** ***********************************************************************
  * This file is part of OscaR (Scala in OR).
  *
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/gpl-3.0.html
  * ****************************************************************************
  */

package oscar.cp.constraints

import oscar.search._
import oscar.cp.core._

/** @author Pierre Schaus pschaus@gmail.com
  */
class CPObjective(val objVars: CPVarInt*) extends Constraint(objVars(0).store, "objective") with Objective {

	def this(x: CPVarInt) = this(Array(x): _*)

	val bestObjs = Array.fill(objVars.size)(0)
	protected val bounds = Array.fill(objVars.size)(0)

	protected val tightenedOnce = Array.fill(objVars.size)(false)

	private var currObj = 0

	def currentObjectiveIdx = currObj

	def currentObjective = objVars(currentObjectiveIdx)

	def currentObjective_=(i: Int) = {
		if (i > objVars.size || i < 0) {
			throw new IllegalArgumentException("objective index between " + 0 + " and " + (objVars.size - 1))
		}
		restoreBest()
		currObj = i

		if (tightenedOnce(i)) {
			reTighten()
		}
	}

	def tighten() = {}

	def reTighten() = {}

	def relax() = {}

	def restoreBest() = {}

	def isOptimum() = false

	/** set the bound of the current objective to value
	  */
	def bound_=(value: Int) = {
		bounds(currentObjectiveIdx) = value
	}

	/** get the bound of the current objective
	  */
	def bound = bounds(currentObjectiveIdx)

	/** set the current best objective found so far
	  */
	def bestObj_=(value: Int) = {
		bestObjs(currentObjectiveIdx) = value
	}

	/** get the current best objective found so far
	  */
	def bestObj = bestObjs(currentObjectiveIdx)

	def isOK() = s.propagate(this) != CPOutcome.Failure

	// constraint methods

	override def setup(l: CPPropagStrength): CPOutcome = {
		objVars.foreach(_.callPropagateWhenBoundsChange(this))
		propagate()
	}

}
