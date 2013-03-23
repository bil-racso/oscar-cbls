/**
 * ***********************************************************************
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
import oscar.cp.modeling._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPObjective(val st: Store, val objs: CPObjectiveUnit*) extends Constraint(st, "objective constraint") with Objective {

  def map = objs.map(o => o.objVar -> o).toMap

  def this(s: Store, o: Array[CPObjectiveUnit]) = this(s, o: _*)
  def tighten() = objs.foreach(_.tighten())

  def isOptimum() = objs.forall(_.isOptimum())

  def isOK() = {
    val objCons = objs.toArray
    propagate != CPOutcome.Failure
  }

  def apply(objVar: CPVarInt) = map(objVar)

  def diversify() {
    for (o <- objs) {
      o.tightenMode = TightenType.NoTighten
    }
  }

  def intensify(sol: CPSol, objVar: CPVarInt) {
    for (o <- objs) {
      o.best = sol(o.objVar)
      o.tightenMode = if (o.objVar == objVar) TightenType.StrongTighten else TightenType.WeakTighten
    }
  }

  def intensify(sol: CPSol) {
    for (o <- objs) {
      o.best = sol(o.objVar)
      o.tightenMode = TightenType.StrongTighten
    }
  }

  // constraint methods

  override def propagate(): CPOutcome = {
    if (objs.forall(_.ensureBest() != CPOutcome.Failure)) CPOutcome.Suspend
    else CPOutcome.Failure
  }

  override def setup(l: CPPropagStrength): CPOutcome = propagate()

  override def toString = objs.mkString(" , ")
}
