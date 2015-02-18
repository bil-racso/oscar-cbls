package oscar.lcg.variables

import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.core.CDCLStore
import oscar.cp.core.CPStore
import oscar.lcg.core.Literal

/** @author Renaud Hartert ren.hartert@gmail.com */
class LCGIntervalViewOffset(variable: LCGIntervalVar, offset: Int, final override val name: String) extends LCGIntervalVar {

  /** Return the CP Store. */
  def cpStore: CPStore = variable.cpStore

  /** Return the LCG store. */
  def cdclStore: CDCLStore = variable.cdclStore

  /** Update the domain and notify the constraints. */
  def updateAndNotify(): Unit = variable.updateAndNotify()

  /** Return the minimum value in the domain. */
  def min: Int = variable.min + offset

  /** Return the maximum value in the domain. */
  def max: Int = variable.max + offset

  /** Return the size of the domain. */
  def size: Int = variable.size

  /** Return true if the variable is assigned. */
  def isAssigned: Boolean = variable.isAssigned

  /** Return true if the variable is assigned to value. */
  def isAssignedTo(value: Int): Boolean = variable.isAssignedTo(value - offset)

  /** Return true if the domain contains value.*/
  def contains(value: Int): Boolean = variable.isAssignedTo(value - offset)

  /** Return the literal `value <= this`. */
  def greaterEqual(value: Int): Literal = variable.greaterEqual(value - offset)

  /** Return the literal `this <= value`. */
  def lowerEqual(value: Int): Literal = variable.lowerEqual(value - offset)

  /** Register the constraint on bounds changes. */
  def callWhenBoundsChange(constraint: LCGConstraint): Unit = variable.callWhenBoundsChange(constraint)

  /** Register the constraint on assignments. */
  def callWhenAssigned(constraint: LCGConstraint): Unit = variable.callWhenAssigned(constraint)
}