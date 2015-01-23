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
 * *****************************************************************************/

package oscar.cp.searches

import oscar.cp._
import oscar.algo.search.Branching
import oscar.algo.reversible.ReversibleInt

/**
 * Abstract Binary Branching:
 * You can specify your variable heuristics
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class AbstractBinaryBranching[X <: CPIntervalVar](variables: Array[X], varHeuris: (X => Int)) extends Branching {

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

  protected def nextVar(): X = {
    var i = nBounds.value
    var bestId = indexes(i)
    var bestVariable = variables(bestId)
    var bestH = varHeuris(bestVariable)
    i += 1
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else {
        val h = varHeuris(variable)
        if (h < bestH || (h == bestH && varId < bestId)) {
          bestVariable = variable
          bestId = varId
          bestH = h
        }
      }
      i += 1
    }
    bestVariable
  }

  def alternatives(): Seq[Alternative]
}
/**
 * Binary Branching:
 * You can specify your variable/value heuristics
 * author: Pierre Schaus pschaus@gmail.com
 */
class BinaryBranching[X <: CPIntVar](vars: Array[X], varHeuris: (CPIntVar => Int), valHeuris: (CPIntVar => Int) = minVal) extends AbstractBinaryBranching(vars, varHeuris) {
  final override def alternatives(): Seq[Alternative] = {
    val stop = allBounds()
    if (stop) noAlternative
    else {
      val variable = nextVar()
      val value = valHeuris(variable)
      branch(cp.assign(variable, value))(cp.remove(variable, value))
    }
  }
}

class BinaryStaticOrderBranching(vars: Array[_ <: CPIntVar], valHeuris: (CPIntVar => Int) = minVal) extends Branching {

  val cp = vars(0).store
  var y = vars.asInstanceOf[Array[CPIntVar]]
  var i = new ReversibleInt(cp, 0)

  final override def alternatives(): Seq[Alternative] = {

    while (i.value < y.size && y(i.value).isBound) { i.incr() }

    if (i.value < y.size) {

      val x: CPIntVar = y(i.value)
      val v = valHeuris(x)
      branch {
        cp.assign(x, v)
      } {
        cp.remove(x, v)
      }

    } else {
      noAlternative
    }
  }
}

/**
 * Binary First Fail (min dom size) on the decision variables vars.
 * @param vars: the array of variables to assign during the search
 * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
 */
class BinaryFirstFailBranching(x: Array[CPIntVar], valHeuris: (CPIntVar => Int) = minVal) extends BinaryBranching(x, _.size, valHeuris) {
  def this(x: CPIntVar*) = this(x.toArray)
}

/**
 * Binary search on the decision variables vars, selecting first the variables having the max number
 * of propagation methods attached to it.
 */
class BinaryMaxDegreeBranching(x: Array[CPIntVar]) extends BinaryBranching(x, varHeuris = maxDegree, valHeuris = minVal)

/**
 * Binary search on the decision variables vars, splitting the domain at the selected value (left : <= value, right : > value)
 */
class BinaryDomainSplitBranching[X <: CPIntervalVar](x: Array[X], varHeuris: (X => Int), valHeuris: (X => Int) = (x: X) => (x.min + x.max) / 2) extends AbstractBinaryBranching(x, varHeuris) {

  override def alternatives(): Seq[Alternative] = {
    allBounds() match {
      case true => noAlternative
      case false => {
        val x = nextVar()
        val value = valHeuris(x)
        branch(cp.post(x <= value))(cp.post(x > value))
      }
    }
  }
}

class BinarySetBranching(x: CPSetVar) extends Branching {
  val cp = x.store
  def alternatives(): Seq[Alternative] = {
    if (x.isBound) noAlternative
    else {
      val v = x.arbitraryPossibleNotRequired
      branch(cp.post(x ++ v))(cp.post(x -- v))
    }
  }
}
