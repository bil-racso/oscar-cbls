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

package oscar.cp.searches

import oscar.cp._
import oscar.algo.reversible._
import oscar.algo.search.Branching
import oscar.cp.core.CPOutcome.Failure

/**
 * Last Conflict Search with Simple phase saving:
 * at first, branch by assign/remove.
 * Remember the last value tried on every variable in phase.
 * Use phase(i), which is the last value that did not fail on assign,
 * to guide the assignment/split chosen for branching.
 *
 * @author Steven Gay
 * @author Renaud Hartert
 */

class SplitLastConflict(variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends Branching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length
  private[this] val store = variables(0).store

  // Order in which variables have to be assigned
  private[this] val order = Array.tabulate(nVariables) { i => i }

  // Last successfuly assigned value for each variable
  private[this] val lastValues = Array.tabulate(nVariables) { i => Int.MinValue }

  // Current depth of the search tree
  private[this] val depthRev = new ReversibleInt(store, 0)

  // Maximum number of assigned variables
  private[this] var maxDepth: Int = -1

  // Depth in which the last conflict occured
  private[this] var conflictDepth: Int = -1

  private[this] var lastDepth = Int.MaxValue
  private[this] var myMin = Int.MaxValue
  private[this] var myMax = Int.MinValue
  private[this] var swapDepth = Int.MinValue

  final override def reset(): Unit = maxDepth = -1

  final override def alternatives: Seq[Alternative] = {
    val depth = updateAssigned()
    if (depth >= nVariables) noAlternative
    else {
      // Trail the depth of the search tree
      depthRev.value = depth

      // Adjust variables order according to the last conflict
      if (conflictDepth > depth && !variables(order(conflictDepth)).isBound) {

        val swap = if (depth < lastDepth) {
          lastDepth = depth
          val swap = myMax
          myMax = Int.MinValue
          swap
        } else conflictDepth
          
          
        // Assign the last conflicting variable first
        val varId = order(depth)
        System.arraycopy(order, depth + 1, order, depth, swap - depth)
        order(swap) = varId
        //println(order.map(variables(_).name).mkString(" "))
        
        conflictDepth = -1
      } else if (depth > maxDepth) {
        // New depth level
        maxDepth = depth
        val position = nextVariable(depth)
        val varId = order(position)
        order(position) = order(depth)
        order(depth) = varId
      }

      // Variable and value
      val varId = order(depth)
      val variable = variables(varId)
      val minValue = variable.min
      val maxValue = variable.max
      val lastValue = lastValues(varId)
      val value = if (minValue <= lastValue && lastValue <= maxValue) lastValue else valHeuristic(varId)

      // Alternatives
      if (maxValue == value) List(assign(variable, value, depth), lower(variable, value, depth))
      else if (minValue == value) List(assign(variable, value, depth), greater(variable, value, depth))
      else List(assign(variable, value, depth), lower(variable, value, depth), greater(variable, value, depth))
    }
  }

  // Return an Alternative that assign the value to the variable
  @inline private def assign(variable: CPIntVar, value: Int, depth: Int): Alternative = () => {
    val out = store.assign(variable, value)
    if (out == Failure) {
      conflictDepth = depth
      if (depth > myMax) myMax = depth
    }
  }

  // Return an Alternative that constraints the variable to be greater than value
  @inline private def greater(variable: CPIntVar, value: Int, depth: Int): Alternative = () => {
    variable.updateMin(value + 1)
    val out = store.propagate()
    if (out == Failure) {
      conflictDepth = depth
      if (depth > myMax) myMax = depth
    }
  }

  // Return an Alternative that constraints the variable to be lower than value
  @inline private def lower(variable: CPIntVar, value: Int, depth: Int): Alternative = () => {
    variable.updateMax(value - 1)
    val out = store.propagate()
    if (out == Failure) {
      conflictDepth = depth
      if (depth > myMax) myMax = depth
    }
  }

  @inline private def updateAssigned(): Int = {
    var d = depthRev.value
    while (d < nVariables && variables(order(d)).isBound) {
      val varId = order(d)
      lastValues(varId) = variables(varId).min
      d += 1
    }
    d
  }

  @inline private def nextVariable(depth: Int): Int = {
    var minId = depth
    var min = Int.MaxValue
    var i = depth
    while (i < nVariables) {
      val varId = order(i)
      if (!variables(varId).isBound) {
        val m = varHeuristic(order(i))
        if (m < min) {
          min = m
          minId = i
        }
      }
      i += 1
    }
    minId
  }
}
