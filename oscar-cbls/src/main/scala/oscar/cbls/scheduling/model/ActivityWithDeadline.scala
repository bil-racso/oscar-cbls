/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Yoann Guyot
 * ****************************************************************************
 */

package oscar.cbls.scheduling.model

import oscar.cbls.invariants.core.computation.{ CBLSSetVar, CBLSIntVar }
import scala.collection.immutable.SortedSet
import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.lib.minmax.MaxArray
import oscar.cbls.invariants.lib.minmax.Max2

/**
 * @author yoann.guyot@cetic.be
 * THIS IS EXPERIMENTAL
 */
class ActivityWithDeadline(
  duration: CBLSIntVar,
  planning: Planning with Deadlines,
  name: String = "",
  shifter: (CBLSIntVar, CBLSIntVar) => CBLSIntVar = (a: CBLSIntVar, _) => a)
  extends Activity(duration, planning, name, shifter) {

  val deadline: CBLSIntVar = 0
  val weight: CBLSIntVar = 1
  val tardiness: CBLSIntVar = Max2(0, (earliestEndDate - deadline) * weight)

  planning.addActivityWithDeadline(this)

  def setDeadline(dueDate: CBLSIntVar, weightingFactor: CBLSIntVar) {
    deadline <== dueDate
    weight <== weightingFactor
  }

  def isLate: Boolean = tardiness.value > 0
}
