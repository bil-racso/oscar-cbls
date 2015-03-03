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
  private[this] val lastValues = Array.tabulate(nVariables) { i => valHeuristic(i) }

  // Current depth of the search tree
  private[this] val depthRev = new ReversibleInt(store, 0)

  // Maximum number of assigned variables
  private[this] var maxDepth: Int = -1

  // Depth in which the last conflict occured
  private[this] var conflictDepth: Int = -1

  final override def reset(): Unit = maxDepth = -1

  final override def alternatives: Seq[Alternative] = {
    val depth = currentDepth()
    if (depth >= nVariables) noAlternative
    else {
      // Trail the depth of the search tree
      depthRev.value = depth

      // Adjust variables order according to the last conflict
      if (conflictDepth > depth && !variables(order(conflictDepth)).isBound) {
        // Assign the last conflicting variable first
        val varId = order(conflictDepth)
        System.arraycopy(order, depth, order, depth + 1, conflictDepth - depth)
        order(depth) = varId
        conflictDepth = depth
      } else if (depth > maxDepth) {
        // New depth level
        maxDepth = depth
        val position = nextVariable(depth)
        val varId = order(position)
        order(position) = order(depth)
        order(depth) = varId
        conflictDepth = depth
      }

      // Variable and value
      val varId = order(depth)
      val variable = variables(varId)
      val minValue = variable.min
      val maxValue = variable.max
      val lastValue = lastValues(varId)
      val value = if (minValue <= lastValue && lastValue <= maxValue) lastValue else valHeuristic(varId)

      // Alternatives
      if (minValue == value) List(assignValue(value, variable), splitRight(value, variable))
      else if (value == maxValue) List(assignValue(value, variable), splitLeft(value, variable))
      else List(assignValue(value, variable), splitLeft(value, variable), splitRight(value, variable))
    }
  }

  // Return an Alternative that assign the value to the variable
  @inline private def assignValue(value: Int, variable: CPIntVar): Alternative = () => {
    Decision.assign(variable, value)
  }

  // Return an Alternative that constraints the variable to be greater than value
  @inline private def splitRight(value: Int, variable: CPIntVar): Alternative = () => {
    Decision.greaterEq(variable, value + 1)
  }

  // Return an Alternative that constraints the variable to be lower than value
  @inline private def splitLeft(value: Int, variable: CPIntVar): Alternative = () => {
    Decision.lowerEq(variable, value - 1)
  }

  @inline private def currentDepth(): Int = {
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
