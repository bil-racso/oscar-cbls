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

package oscar.cp.searches

import oscar.cp._
import oscar.algo.search.Branching
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversibleIntWithCache

/**
 * Abstract Binary Branching:
 * You can specify your variable heuristics
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 *
 * @param varHeuris is a variable heuristic, it will select preferably first the unbound
 *        variables(i) such that varHeuris(i) is the smallest
 */
abstract class AbstractBinaryBranching(variables: Array[CPIntVar], varHeuris: (Int => Int)) extends Branching {

  val cp = variables(0).store

  private[this] val nVariables = variables.length
  private[this] val indexes = Array.tabulate(nVariables)(i => i)
  private[this] val nBounds = new ReversibleInt(cp, 0)

  @inline private def bound(i: Int): Unit = {
    val id = nBounds.incr() - 1
    val tmp = indexes(id)
    indexes(id) = indexes(i)
    indexes(i) = tmp
  }

  protected def allBounds(): Boolean = {
    var i = nBounds.value
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else return false
      i += 1
    }
    true
  }

  protected def nextVar(): Int = {
    var i = nBounds.value
    var bestId = indexes(i)
    var bestVariable = variables(bestId)
    var bestH = varHeuris(bestId)
    i += 1
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else {
        val h = varHeuris(varId)
        if (h < bestH || (h == bestH && varId < bestId)) {
          bestVariable = variable
          bestId = varId
          bestH = h
        }
      }
      i += 1
    }
    bestId
  }

  def alternatives(): Seq[Alternative]
}
/**
 * Binary Branching:
 * You can specify your variable/value heuristics
 * author: Pierre Schaus pschaus@gmail.com
 */
class BinaryBranching(variables: Array[CPIntVar], varHeuris: (Int => Int), valHeuris: (Int => Int)) extends AbstractBinaryBranching(variables, varHeuris) {

  def this(vars: Array[CPIntVar], varHeuris: (Int => Int)) = this(vars, varHeuris, minVal(vars))

  final override def alternatives(): Seq[Alternative] = {
    if (allBounds()) noAlternative
    else {
      val i = nextVar()
      val variable = variables(i)
      val value = valHeuris(i)
      branch(cp.assign(variable, value))(cp.remove(variable, value))
    }
  }
}

class BinaryStaticOrderBranching(variables: Array[CPIntVar], valHeuris: (Int => Int)) extends Branching {

  def this(vars: Array[CPIntVar]) = this(vars, minVal(vars))

  private[this] val store = variables(0).store
  private[this] val nVariables = variables.length
  private[this] val depthRev = new ReversibleIntWithCache(store, 0, nVariables + 1)
  private[this] var depth = 0

  final override def alternatives(): Seq[Alternative] = {
    // Cache
    depth = depthRev.value

    // Update depth 
    while (depth < nVariables && variables(depth).isBound) depth += 1

    if (depth == nVariables) noAlternative
    else {
      // Trail new depth
      depthRev.value = depth
      // Alternatives
      val variable = variables(depth)
      val value = valHeuris(depth)
      branch { store.assign(variable, value) } { store.remove(variable, value) }
    }
  }
}

/**
 * Binary First Fail (min dom size) on the decision variables vars.
 * @param vars: the array of variables to assign during the search
 * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
 */
class BinaryFirstFailBranching(x: Array[CPIntVar], valHeuris: (Int => Int)) extends BinaryBranching(x, i => x(i).size, valHeuris) {

  def this(x: Array[CPIntVar]) = this(x, minVal(x))

  def this(x: CPIntVar*) = this(x.toArray)
}

/**
 * Binary search on the decision variables vars, selecting first the variables having the max number
 * of propagation methods attached to it.
 */
class BinaryMaxDegreeBranching(x: Array[CPIntVar]) extends BinaryBranching(x, varHeuris = i => x(i).constraintDegree, valHeuris = minVal(x))

/**
 * Binary search on the decision variables vars, splitting the domain at the selected value (left : <= value, right : > value)
 */
class BinaryDomainSplitBranching(variables: Array[CPIntVar], varHeuris: (Int => Int), valHeuris: (Int => Int)) extends AbstractBinaryBranching(variables, varHeuris) {

  def this(x: Array[CPIntVar], varHeuris: (Int => Int)) = this(x, varHeuris, i => (x(i).min + x(i).max) / 2)

  override def alternatives(): Seq[Alternative] = {
    if (allBounds()) noAlternative
    else {
      val i = nextVar()
      val variable = variables(i)
      val value = valHeuris(i)
      branch(cp.post(variable <= value))(cp.post(variable > value))
    }
  }
}

class BinarySetBranching(x: CPSetVar) extends Branching {
  private[this] val store = x.store
  def alternatives(): Seq[Alternative] = {
    if (x.isBound) noAlternative
    else {
      val v = x.arbitraryPossibleNotRequired
      branch(store.post(x ++ v))(store.post(x -- v))
    }
  }
}
