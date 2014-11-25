/*******************************************************************************
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
 ******************************************************************************/

package oscar.cp.scheduling.search

import oscar.cp.modeling._
import oscar.cp.core.CPIntVar
import oscar.algo.reversible._
import oscar.algo.search.Branching

/**
 * Set Times Branching
 * author: Pierre Schaus pschaus@gmail.com
 */
class SetTimesBranching(starts: IndexedSeq[CPIntVar], durations: IndexedSeq[CPIntVar], ends: IndexedSeq[CPIntVar], tieBreaker: Int => Int = (i: Int) => i) extends Branching {

  val cp = starts.head.store
  val n = starts.size
  val Activities = 0 until n

  val selectable = Array.fill(n)(new ReversibleBool(cp, true))
  // non fixed activities (by setTimes)
  val bound = Array.fill(n)(new ReversibleBool(cp, false))

  val oldEST = Array.fill(n)(new ReversibleInt(cp, -1))

  // update the new ones becoming available because est has moved
  def updateSelectable() = (Activities).filter(i => oldEST(i).value < starts(i).min || durations(i).max == 0).foreach(selectable(_).value = true)
  def selectableIndices() = (Activities).filter(i => selectable(i).value && !bound(i).value)
  def allStartBounds() = bound.forall(i => i.value)

  def updateAndCheck() = {
    updateSelectable()
    if (selectableIndices().isEmpty && !allStartBounds()) cp.fail()
  }

  def alternatives(): Seq[Alternative] = {
    if (allStartBounds()) {
      noAlternative
    } else {
      updateSelectable()
      val (est, ect,x) = selectableIndices().map(i => (starts(i).min, tieBreaker(i),i)).min
      // Select the activity with the smallest EST, ECT as tie breaker
      branch {
        cp.post(starts(x) == est)
        bound(x).value = true
        if (!cp.isFailed) updateAndCheck()
      } {
        cp.propagate()
        selectable(x).value = false
        oldEST(x).value = est
        updateAndCheck()
      }
    }
    
  }
}

