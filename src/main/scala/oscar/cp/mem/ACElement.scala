package oscar.cp.mem

import scala.Math.min
import scala.Math.max

import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints.Eq
import oscar.cp.modeling.CPSolver
import oscar.reversible.ReversibleInt
import oscar.reversible.ReversibleSetIndexedArray

/**ACElement
 *
 * A full Arc-Consistent Element Constraint: y(x) == z
 *
 * @author Renaud Hartert - ren.hartert@gmail.com
 */
class ACElement(cp: Store, y: Array[CPVarInt], x: CPVarInt, z: CPVarInt) extends Constraint(cp, "ACElement") {
  
  private val xRange = max(0, x.min) to min(x.max, y.size)
  private val zRange = z.min to z.max

  // Number of supports for the value v i.e number of indices i such that v is in y(i)
  private val _nSupports = Array.fill(zRange.size)(new ReversibleInt(cp, 0))
  // For all indices i in x: intersect(i) is the size of the intersection between y(i) and z
  private val _intersect = Array.fill(xRange.size)(new ReversibleSetIndexedArray(cp, z.min, z.max, true))
  
  // Mapping functions used to limit the size of both previous structures
  private def nSupports(i: Int) = _nSupports(i-zRange.min)
  private def intersect(i: Int) = _intersect(i-xRange.min)

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (adjustX() == Failure) Failure
    else {
      val out = propagate()
      if (out == Failure || out == Success) out
      else {
        x.callValRemoveWhenValueIsRemoved(this)
        z.callValRemoveWhenValueIsRemoved(this)
        for (i <- x.min to x.max; if x hasValue i) {
          y(i).callValRemoveIdxWhenValueIsRemoved(this, i)
        }
        Suspend
      }
    }
  }

  override def propagate(): CPOutcome = {
    resetData() // Mandatory if propagate is called after the initial call
    initData()

    for (i <- x.min to x.max; if x hasValue i) {
      if (intersect(i).getSize == 0) {
        if (x.removeValue(i) == Failure) return Failure
      }
    }
    if (x.isBound) return bindX()

    for (v <- z.min to z.max; if z hasValue v) {
      if (nSupports(v).value == 0) {
        if (z.removeValue(v) == Failure) return Failure
      }
    }
    Suspend
  }

  override def valRemoveIdx(cpvar: CPVarInt, i: Int, v: Int): CPOutcome = removeFromY(i, v)

  override def valRemove(cpvar: CPVarInt, v: Int): CPOutcome = {
    if (cpvar == x) removeFromX(v)
    else removeFromZ(v)
  }

  // Initializes data structures
  private def initData() {
    for (i <- x.min to x.max; if x hasValue i) {
      for (v <- y(i).min to y(i).max; if y(i) hasValue v) {
        if (z hasValue v) {
          nSupports(v).incr()
          intersect(i) insert v
        }
      }
    }
  }
  
  // Reset the content of both data structures
  private def resetData() {
    for (i <- 0 until _intersect.size)
      _intersect(i).empty()
    for (v <- 0 until _nSupports.size)
      _nSupports(v).setValue(0)
  }

  // Reduces the number of supports of the value v
  private def reduceSupports(v: Int): CPOutcome = {
    nSupports(v).decr()
    if (nSupports(v).value == 0) z.removeValue(v)
    else Suspend
  }

  // Removes the value v from the intersection between y(i) and z
  private def reduceIntersect(i: Int, v: Int): CPOutcome = {
    intersect(i).removeValue(v)
    if (intersect(i).getSize == 0) x.removeValue(i)
    else Suspend
  }

  // Removes v from all the intersections
  private def removeFromZ(v: Int): CPOutcome = {
    nSupports(v) setValue 0
    for (i <- x.min to x.max; if x hasValue i; if intersect(i) hasValue v) {
      if (reduceIntersect(i, v) == Failure) return Failure
    }
    Suspend
  }

  // If x is bound, this constraint is replaced by an Equality constraint
  // else, the number of supports for all values v in y(i) is reduced by 1
  private def removeFromX(i: Int): CPOutcome = {
    if (x.isBound) bindX()
    else {
      intersect(i).empty()
      for (v <- y(i).min to y(i).max; if y(i) hasValue v; if v < zRange.max) {
        if (reduceSupports(v) == Failure) return Failure
      }
      Suspend
    }
  }

  // If y(i) has an intersection with z, the number of supports of the 
  // value v is reduced by 1
  private def removeFromY(i: Int, v: Int): CPOutcome = {
    if (intersect(i).getSize > 0 && intersect(i).hasValue(v)) {
      if (reduceSupports(v) == Failure) Failure
      else reduceIntersect(i, v)
    } else Suspend
  }

  // Replaces this constraint by an Equality constraint
  private def bindX(): CPOutcome = {
    if (cp.post(new Eq(y(x.value), z)) == Failure) Failure
    else Success
  }

  // Removes each value i in x that is not a valid id in y
  private def adjustX(): CPOutcome = {
    if (x.updateMin(0) == Failure) Failure
    else if (x.updateMax(y.size - 1) == Failure) Failure
    else if (x.isBound) bindX()
    else Suspend
  }
}

object ACElement {

  def apply(y: Array[CPVarInt], x: CPVarInt): CPVarInt = {
    val cp = x.store
    val max = y.map(_.max).max
    val min = y.map(_.min).min
    val z = CPVarInt(cp, min to max)
    cp.add(new ACElement(cp, y, x, z))
    z
  }

  def apply(y: Array[CPVarInt], x: CPVarInt, z: CPVarInt) = new ACElement(x.store, y, x, z)
}
