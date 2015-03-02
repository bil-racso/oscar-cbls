package oscar.cp.searches

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

import oscar.cp._
import oscar.algo.reversible._
import oscar.algo.search.Branching
import oscar.cp.core.CPOutcome.Failure

/**
 * Last Conflict Search
 * 
 * Last confict is a first-fail best-firt search.
 *  
 * Variables are firstly assigned according to varHeuristic 
 * and then to the last conflict scheme.
 * 
 * Each variable is assigned to its last successfuly assigned value
 * or the value returned by valHeuristic
 *
 * @author Steven Gay
 * @author Renaud Hartert
 */

class BinaryLastConflict(variables: Array[CPIntVar], varHeuristic: Int => Int, valHeuristic: Int => Int) extends Branching {

  require(variables.length > 0, "no variable")

  private[this] val nVariables = variables.length
  private[this] val store = variables(0).store

  // Order in which variables have to be assigned
  private[this] val order = Array.tabulate(nVariables) { i => i }

  // Last successful assigned value for each variable
  private[this] val lastValues = new Array[Int](nVariables)

  // Current depth of the search tree
  private[this] val depthRev = new ReversibleInt(store, 0)

  // Last conflict
  private[this] var maxDepth: Int = -1
  private[this] var deepestVar: Int = 0

  final override def reset(): Unit = maxDepth = -1

  final override def alternatives: Seq[Alternative] = {
    val depth = currentDepth  
    if (depth >= nVariables) noAlternative
    else {

      // Trail the new depth
      depthRev.value = depth
      
      // Select the variable implied in the last conflict if any
      if (deepestVar > depth && !variables(order(deepestVar)).isBound) {
        val deepestId = order(deepestVar)
        // Insert the last conflict in the sequence
        System.arraycopy(order, depth, order, depth + 1, deepestVar - depth)
        order(depth) = deepestId
      } 
      // Select the next variable suggested by the variable heuristic
      else if (depth > maxDepth) {
        maxDepth = depth
        deepestVar = nextVariable(depth)
        val varId = order(deepestVar)
        lastValues(varId) = valHeuristic(varId)
        // Swap the next variable
        order(deepestVar) = order(depth)
        order(depth) = varId
      }

      deepestVar = depth

      val varId = order(depth)
      val variable = variables(varId)
      val lastValue = lastValues(varId)
      val value = if (variable.hasValue(lastValue)) lastValue else valHeuristic(varId)
      // Alternatives
      List(() => store.assign(variable, value), () => store.remove(variable, value))
    }
  }

  @inline private def currentDepth: Int = {
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
