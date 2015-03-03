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
abstract class AbstractBinaryBranching(variables: Array[CPIntVar], var varHeuris: (Int => Int)) extends Branching {

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
class BinaryBranching(vars: Array[CPIntVar], varHeuris: (Int => Int), valHeuris: (Int => Int)) extends AbstractBinaryBranching(vars, varHeuris) {
  
  def this(vars: Array[CPIntVar], varHeuris: (Int => Int)) = this(vars,varHeuris,minVal(vars))
  
  final override def alternatives(): Seq[Alternative] = {
    val stop = allBounds()
    if (stop) noAlternative
    else {
      val i = nextVar()
      val value = valHeuris(i)
      branch(cp.assign(vars(i), value))(cp.remove(vars(i), value))
    }
  }
}

class BinaryStaticOrderBranching(vars: Array[CPIntVar], valHeuris: (Int => Int)) extends Branching {

  def this(vars: Array[CPIntVar]) = this(vars,minVal(vars))
  
  val cp = vars(0).store
  var y = vars.asInstanceOf[Array[CPIntVar]]
  var i = new ReversibleIntWithCache(cp,0,vars.size+1)

  final override def alternatives(): Seq[Alternative] = {

    while (i.value < y.size && y(i.value).isBound) { i.incr() }

    if (i.value < y.size) {

      val x: CPIntVar = y(i.value)
      val v = valHeuris(i.value)
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
class BinaryFirstFailBranching(x: Array[CPIntVar], valHeuris: (Int => Int)) extends BinaryBranching(x, i => x(i).size, valHeuris) {
  
  def this(x: Array[CPIntVar]) = this(x,minVal(x))
  
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
class BinaryDomainSplitBranching(val x: Array[CPIntVar], varHeuris: (Int => Int), valHeuris: (Int => Int)) extends AbstractBinaryBranching(x, varHeuris) {

  
  def this(x: Array[CPIntVar], varHeuris: (Int => Int)) = this(x,varHeuris,i => (x(i).min + x(i).max) / 2)
  
  
  override def alternatives(): Seq[Alternative] = {
    allBounds() match {
      case true => noAlternative
      case false => {
        val i = nextVar()
        val value = valHeuris(i)
        branch(cp.post(x(i) <= value))(cp.post(x(i) > value))
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
