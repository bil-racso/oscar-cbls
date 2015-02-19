package oscar.lcg.variables

import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.core.CDCLStore
import oscar.cp.core.CPStore
import oscar.lcg.core.Literal

/** @author Renaud Hartert ren.hartert@gmail.com */
class LCGIntervalViewOffset(variable: LCGIntervalVar, offset: Int, final override val name: String) extends LCGIntervalVar {

  /** Return the CP Store. */
  final override def cpStore: CPStore = variable.cpStore

  /** Return the LCG store. */
  final override def cdclStore: CDCLStore = variable.cdclStore

  /** Update the domain and notify the constraints. */
  final override def updateAndNotify(): Unit = variable.updateAndNotify()

  /** Return the minimum value in the domain. */
  @inline final override def min: Int = variable.min + offset

  /** Return the maximum value in the domain. */
  @inline final override def max: Int = variable.max + offset

  /** Return the size of the domain. */
  @inline final override def size: Int = variable.size

  /** Return true if the variable is assigned. */
  @inline final override def isAssigned: Boolean = variable.isAssigned

  /** Return true if the variable is assigned to value. */
  final override def isAssignedTo(value: Int): Boolean = variable.isAssignedTo(value - offset)

  /** Return true if the domain contains value.*/
  final override def contains(value: Int): Boolean = variable.isAssignedTo(value - offset)

  /** Return the literal `value <= this`. */
  final override def greaterEqual(value: Int): Literal = variable.greaterEqual(value - offset)

  /** Return the literal `this <= value`. */
  final override def lowerEqual(value: Int): Literal = variable.lowerEqual(value - offset)

  /** Register the constraint on bounds changes. */
  final override def callWhenBoundsChange(constraint: LCGConstraint): Unit = variable.callWhenBoundsChange(constraint)

  /** Register the constraint on assignments. */
  final override def callWhenAssigned(constraint: LCGConstraint): Unit = variable.callWhenAssigned(constraint)
  
  final override def toString: String = {
    if (variable.size == 1) variable.min.toString
    else s"[${min}, ${max}]"
  }
}