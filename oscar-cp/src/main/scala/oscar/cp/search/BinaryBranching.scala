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

package oscar.cp.search

import oscar.cp.modeling._
import oscar.cp.core.CPIntVar
import oscar.algo.reversible._
import oscar.algo.search.Branching
import oscar.cp.core.CPSetVar

/**
 * Binary Branching: 
 * You can specify your variable/value heuristics
 * author: Pierre Schaus pschaus@gmail.com
 */
class BinaryBranching(vars: Array[_ <: CPIntVar], varHeuris: (CPIntVar => Int), valHeuris: (CPIntVar => Int) = minVal) extends Branching {
  val cp = vars(0).store
  val x_ = vars.asInstanceOf[Array[CPIntVar]].zipWithIndex
  val nbBounds = new ReversibleInt(cp, 0)
  def bound(i: Int) {
    val ind = nbBounds.value
    val tmp = x_(ind)
    x_(ind) = x_(i)
    x_(i) = tmp
    nbBounds.incr()
  }
  val size = x_.size

  def allBounds(): Boolean = {
    var i = nbBounds.value
    while (i < size) {
      if (!x_(i)._1.isBound) return false
      else bound(i)
      i += 1
    }
    true
  }

  def nextVar(): CPIntVar = {
    var i = nbBounds.value
    var (x, ind) = x_(i)
    var fbest = varHeuris(x)
    i += 1
    while (i < size) {
      if (!x_(i)._1.isBound) {
        val (y, indy) = x_(i)
        val h = varHeuris(y)
        if (h < fbest || (h == fbest && indy < ind)) {
          x = y
          fbest = h
          ind = indy
        }
      } else {
        bound(i)
      }
      i += 1
    }
    x
  }

  def alternatives(): Seq[Alternative] = {
    allBounds() match {
      case true => noAlternative
      case false => { 
        val y = nextVar()
        val v = valHeuris(y)
        branch(cp.assign(y, v))(cp.remove(y, v)) // right alternative
      }
    }
  }
}

class BinaryStaticOrderBranching(vars: Array[_ <: CPIntVar], valHeuris: (CPIntVar => Int) = minVal) extends Branching {

  val cp = vars(0).store
  var y = vars.asInstanceOf[Array[CPIntVar]]
  var i = new ReversibleInt(cp, 0)

  override def alternatives(): Seq[Alternative] = {
    
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
class BinaryDomainSplitBranching(x: Array[CPIntVar], varHeuris: (CPIntVar => Int) = minVar, valHeuris: (CPIntVar => Int) = (x: CPIntVar) => (x.min + x.max) / 2) extends BinaryBranching(x,varHeuris,minVal) {

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
