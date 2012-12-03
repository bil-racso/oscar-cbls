/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

package oscar.cp.constraints;

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils;
import oscar.reversible.ReversibleInt

/**
 * AC Element Constraint on a 2D array
 * @author Pierre Schaus pschaus@gmail.com
 */
object ElementCst2D {

  private var prevT: Array[Array[Int]] = null
  private var prevData: TableData = null

  def apply(T: Array[Array[Int]], x: CPVarInt, y: CPVarInt, z: CPVarInt) = {
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
 */
class ElementCst2D(T: Array[Array[Int]], x: CPVarInt, y: CPVarInt, z: CPVarInt) extends Constraint(x.s, "ElementCst2D") {

  val sortedTuples = (for (i <- 0 until T.size; j <- 0 until T(i).size) yield (T(i)(j), i, j)).sortBy(t => t).toArray
  val nbColSupports = Array.fill(T.size)(new ReversibleInt(s, 0))
  val nbRowSupports = Array.fill(T(0).size)(new ReversibleInt(s, 0))
  val low = new ReversibleInt(s, 0)
  val up = new ReversibleInt(s, sortedTuples.size - 1)



  def setup(l: CPPropagStrength): CPOutcome = {

    for ((v,i,j) <- sortedTuples) {
      nbColSupports(i).incr()
      nbRowSupports(j).incr()
    }
    if (x.updateMin(0) == CPOutcome.Failure) return CPOutcome.Failure
    if (x.updateMax(T.size-1) == CPOutcome.Failure) return CPOutcome.Failure
    if (y.updateMin(0) == CPOutcome.Failure) return CPOutcome.Failure
    if (y.updateMax(T(0).size-1) == CPOutcome.Failure) return CPOutcome.Failure
    x.callPropagateWhenDomainChanges(this)
    y.callPropagateWhenDomainChanges(this)
    z.callPropagateWhenBoundsChange(this)
    propagate()
  }

  def zvalue(i: Int) = sortedTuples(i)._1
  def xvalue(i: Int) = sortedTuples(i)._2
  def yvalue(i: Int) = sortedTuples(i)._3
  
  
  def xlow = xvalue(low.value)
  def ylow = yvalue(low.value)
  def zlow = zvalue(low.value)
  def xup = xvalue(up.value)
  def yup = yvalue(up.value)
  def zup = zvalue(up.value)

  // entry i disappear
  def update(i: Int): Boolean = {
    nbColSupports(xvalue(i)).decr()
    if (nbColSupports(xvalue(i)).value == 0) {
      if (x.removeValue(xvalue(i)) == CPOutcome.Failure) return false
    }
    nbRowSupports(yvalue(i)).decr()
    if (nbRowSupports(yvalue(i)).value == 0) {
      if (y.removeValue(yvalue(i)) == CPOutcome.Failure) return false
    }
    true
  }
  
  def remainingSupport = up.value - low.value + 1
  def supportLeft = remainingSupport > 0

  override def propagate(): CPOutcome = {
    while (zlow < z.min || !x.hasValue(xlow) || !y.hasValue(ylow)) {
      if (!update(low.value)) return CPOutcome.Failure
      low.incr()
      if (!supportLeft) return CPOutcome.Failure
    }
    while (zup > z.max || !x.hasValue(xup) || !y.hasValue(yup)) {
      if (!update(up.value)) return CPOutcome.Failure
      up.decr()
      if (!supportLeft) return CPOutcome.Failure
    }
    if (z.updateMin(zvalue(low.value)) == CPOutcome.Failure) return CPOutcome.Failure
    if (z.updateMax(zvalue(up.value)) == CPOutcome.Failure) return CPOutcome.Failure
    CPOutcome.Suspend
  }

}
	

