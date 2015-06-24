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

package oscar.cp.constraints;

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils;
import oscar.algo.reversible.ReversibleInt

/**
 * AC Element Constraint on a 2D array
 * @author Pierre Schaus pschaus@gmail.com
 */
object ElementCst2D {

  private var prevT: Array[Array[Int]] = null
  private var prevData: TableData = null

  def apply(T: Array[Array[Int]], x: CPIntVar, y: CPIntVar, z: CPIntVar) = {
    if (prevT != T) {
      prevT = T;
      prevData = new TableData(3);
      for (i <- 0 until T.size; j <- 0 until T(i).size) {
        prevData.add(i, j, T(i)(j));
      }
    }
    new TableAC5TCRecomp(prevData, x, y, z)
  }
}

/**
 * BC Element Constraint on a 2D array
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
final class ElementCst2D(T: Array[Array[Int]], x: CPIntVar, y: CPIntVar, z: CPIntVar) extends Constraint(x.store, "ElementCst2D") {

  require(T.length > 0)
  require(T(0).length > 0)

  private[this] val nRows = T.length
  private[this] val nCols = T(0).length

  private[this] val nTuples = nCols * nRows
  private[this] val sortedTuples = (for (i <- 0 until T.size; j <- 0 until T(i).size) yield Array(T(i)(j), i, j)).sortBy(t => t(0)).transpose.toArray
  private[this] val nbColSupports = Array.fill(nRows)(new ReversibleInt(s, 0))
  private[this] val nbRowSupports = Array.fill(nCols)(new ReversibleInt(s, 0))
  private[this] val lowRev = new ReversibleInt(s, 0)
  private[this] val upRev = new ReversibleInt(s, nTuples - 1)
  
  private[this] val zvalue = sortedTuples(0)
  private[this] val xvalue = sortedTuples(1)
  private[this] val yvalue = sortedTuples(2)

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (x.updateMin(0) == Failure) Failure
    else if (x.updateMax(nRows - 1) == Failure) Failure
    else if (y.updateMin(0) == Failure) Failure
    else if (y.updateMax(nCols - 1) == Failure) Failure
    else {
      init()
      x.callPropagateWhenDomainChanges(this)
      y.callPropagateWhenDomainChanges(this)
      z.callPropagateWhenBoundsChange(this)
      propagate()
    }
  }

  @inline private def init(): Unit = {
    var i = nTuples
    while (i > 0) {
      i -= 1
      nbColSupports(sortedTuples(1)(i)).incr()
      nbRowSupports(sortedTuples(2)(i)).incr()
    }
  }

  // entry i disappear
  @inline private def update(i: Int): Boolean = {
    if (nbColSupports(xvalue(i)).decr() == 0) {
      if (x.removeValue(xvalue(i)) == Failure) return false
    }
    if (nbRowSupports(yvalue(i)).decr() == 0) {
      if (y.removeValue(yvalue(i)) == Failure) return false
    }
    true
  }

  override def propagate(): CPOutcome = {

    // Cache
    var low = lowRev.value
    var up = upRev.value

    while (zvalue(low) < z.min || !x.hasValue(xvalue(low)) || !y.hasValue(yvalue(low))) {
      if (!update(low)) return Failure
      low += 1
      if (up < low) return Failure
    }

    while (zvalue(up) > z.max || !x.hasValue(xvalue(up)) || !y.hasValue(yvalue(up))) {
      if (!update(up)) return Failure
      up -= 1
      if (up < low) return Failure
    }

    if (z.updateMin(zvalue(low)) == Failure) Failure
    else if (z.updateMax(zvalue(up)) == Failure) Failure
    else {
      // Trail
      lowRev.value = low
      upRev.value = up
      Suspend
    }
  }
}
	

