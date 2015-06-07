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

package oscar.cp.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversibleBoolean
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 * @author Sacha Van Cauwelaert sascha.vancauwelaert@gmail.com
 * @author Jordan Demeulenaere
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
final class TableSTR2(variables: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(variables(0).store, "TableSTR2") {

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  private[this] val arity = variables.length
  private[this] val nTuples = table.length

  // Tuples to consider
  private[this] val activeTuples = Array.tabulate(nTuples)(i => i)
  private[this] val nActiveTuplesRev = new ReversibleInt(s, nTuples)
  private[this] var nActiveTuples = 0

  private[this] val isBoundAndChecked = Array.fill(arity)(new ReversibleBoolean(s, false))

  // Stacks used to represent SSup et SVal
  private[this] val sSup = new Array[Int](arity)
  private[this] val sVal = new Array[Int](arity)
  private[this] var sSupSize = 0
  private[this] var sValSize = 0

  // Sparse sets for values to remove
  private[this] val toRemoveValues = Array.tabulate(arity)(i => new Array[Int](variables(i).size))
  private[this] val toRemovePositions = Array.tabulate(arity)(i => new Array[Int](variables(i).max - variables(i).min + 1))
  private[this] val offsets = Array.tabulate(arity)(i => variables(i).min)
  private[this] val sizes = new Array[Int](arity)

  // Last size must be initially different from the domain size
  private[this] val lastSize = Array.fill(arity)(new ReversibleInt(s, -1))

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) Failure
    else {
      var i = arity
      while (i > 0) {
        i -= 1
        if (!variables(i).isBound) variables(i).callPropagateWhenDomainChanges(this)
      }
      Suspend
    }
  }

  override def propagate(): CPOutcome = {

    // Reset SSup and SVal
    sSupSize = 0
    sValSize = 0

    // Cache
    nActiveTuples = nActiveTuplesRev.value

    var i = arity
    while (i > 0) {
      i -= 1
      if (!isBoundAndChecked(i).value) {
        updateSet(i) // Copy the domain of the variable
        sSup(sSupSize) = i
        sSupSize += 1 // push
      }
    }

    i = sSupSize
    while (i > 0) {
      i -= 1
      val varId = sSup(i)
      val varSize = variables(varId).size
      val inSVal = lastSize(varId).value != varSize // changed since last propagate
      lastSize(varId).value = varSize
      if (inSVal) {
        sVal(sValSize) = varId
        sValSize += 1 // push
      }
    }

    i = nActiveTuples
    while (i > 0) {
      i -= 1
      val tau = table(activeTuples(i))
      val isInvalid = isInvalidTuple(tau)
      if (isInvalid) deactivateTuple(i)
      else {
        var j = sSupSize
        while (j > 0) {
          j -= 1
          val varId = sSup(j)
          val newSize = removeFromSet(varId, tau(varId))
          if (newSize == 0) removeFromSSup(j)
        }
      }
    }

    i = sSupSize
    while (i > 0) {
      i -= 1
      val varId = sSup(i)
      val values = toRemoveValues(varId)
      val nValues = sizes(varId)
      val variable = variables(varId)
      val varSize = variable.size

      if (nValues == varSize) return Failure
      else {
        if (nValues > 0) {
          var i = nValues 
          while (i > 0) {
            i -= 1
            val value = values(i)
            variable.removeValue(value)
          }
        }
        if (variable.isBound) isBoundAndChecked(varId).setTrue()
        lastSize(varId).setValue(varSize)
      }
    }

    // Trail only if no Failure
    nActiveTuplesRev.value = nActiveTuples

    Suspend
  }

  @inline private def updateSet(varId: Int): Unit = {
    val variable = variables(varId)
    val size = variable.fillArray(toRemoveValues(varId))
    val values = toRemoveValues(varId)
    val positions = toRemovePositions(varId)
    val offset = offsets(varId)
    sizes(varId) = size
    var i = size
    while (i > 0) {
      i -= 1
      val value = values(i)
      positions(value - offset) = i
    }
  }

  @inline private def removeFromSet(varId: Int, val1: Int): Int = {
    val positions = toRemovePositions(varId)
    val offset = offsets(varId)
    val pos1 = positions(val1 - offset)
    val size = sizes(varId)
    if (pos1 >= size) size
    else {
      val values = toRemoveValues(varId)
      val pos2 = size - 1
      val val2 = values(pos2)
      values(pos1) = val2
      values(pos2) = val1
      positions(val1 - offset) = pos2
      positions(val2 - offset) = pos1
      sizes(varId) = pos2
      pos2
    }
  }

  @inline private def isInvalidTuple(tuple: Array[Int]): Boolean = {
    var i = sValSize
    while (i > 0) {
      i -= 1
      val varId = sVal(i)
      if (!variables(varId).hasValue(tuple(varId))) return true
    }
    false
  }

  @inline private def removeFromSSup(id: Int): Unit = {
    val tmp = sSup(id)
    sSupSize -= 1
    sSup(id) = sSup(sSupSize)
    sSup(sSupSize) = tmp
  }

  @inline private def deactivateTuple(id: Int): Unit = {
    nActiveTuples -= 1
    val tmpPosition = activeTuples(id)
    activeTuples(id) = activeTuples(nActiveTuples)
    activeTuples(nActiveTuples) = tmpPosition
  }
}