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
/**
 * ****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

package oscar.cbls.constraints.lib.basic

import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.constraints.core._
import oscar.cbls.invariants.lib.numeric.{ Abs, Minus }
import oscar.cbls.invariants.core.propagation.Checker
import scala.math.abs

/**
 * implements left <= right
 * @author renaud.delandtsheer@cetic.be
 */
protected class LEA(val left: CBLSIntVar, val right: CBLSIntVar) extends Constraint {
  model = InvariantHelper.findModel(List(left,right))

  registerConstrainedVariables(left, right)

  val Violation: CBLSIntVar = Max2(0, left - right).toIntVar(this.getClass().getSimpleName() + ".violation")

  finishInitialization()

  /**
   * the violation is Max(0,right-left)
   */
  override def violation = Violation

  /**
   * The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Variable): CBLSIntVar = { if (left == v || right == v) Violation else 0 }

  override def checkInternals(c: Checker) {
    val diff = left.value - right.value
    c.check(Violation.value == (if (diff <= 0) 0 else diff),
      Some("Violation.value (" + Violation.value
        + ") == (if (left.value - right.value (" + diff + ") <= 0) 0 else " + diff + ")"))
  }
}

/**
 * implements left <= right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class LE(l: CBLSIntVar, r: CBLSIntVar) extends LEA(l, r)

/**
 * implements left >= right
 * it is just a parameter swap of [[oscar.cbls.constraints.lib.basic.LE]]
 * @author renaud.delandtsheer@cetic.be
 */
case class GE(l: CBLSIntVar, r: CBLSIntVar) extends LEA(r, l)

/**
 * implements left < right
 * @author renaud.delandtsheer@cetic.be
 */
protected class LA(val left: CBLSIntVar, val right: CBLSIntVar) extends Constraint {
  model = InvariantHelper.findModel(List(left,right))
  registerConstrainedVariables(left, right)

  val Violation: CBLSIntVar = Max2(0, left - right + 1)
  finishInitialization(Violation.getPropagationStructure)

  /**
   * the violation is Max(0,left - right + 1)
   */
  override def violation = Violation
  /**
   * The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Variable): CBLSIntVar = { if (left == v || right == v) Violation else 0 }

  override def checkInternals(c: Checker) {
    val diff = left.value - right.value
    c.check(Violation.value == (if (diff < 0) 0 else diff + 1),
      Some("Violation.value (" + Violation.value
        + ") == (if (left.value - right.value (" + diff + ") < 0) 0 else " + (diff + 1) + ")"))
  }
}

/**
 * implements left < right
 * it is just a parameter swap of [[oscar.cbls.constraints.lib.basic.L]]
 * @author renaud.delandtsheer@cetic.be
 */
case class L(l: CBLSIntVar, r: CBLSIntVar) extends LA(l, r)

/**
 * implements left > right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class G(l: CBLSIntVar, r: CBLSIntVar) extends LA(r, l)

/**
 * implements left != right
 * @author renaud.delandtsheer@cetic.be
 */
case class NE(left: CBLSIntVar, right: CBLSIntVar) extends Constraint {
  registerConstrainedVariables(left, right)
  registerStaticAndDynamicDependenciesNoID(left, right)
  finishInitialization()

  val Violation: CBLSIntVar = CBLSIntVar(model, 0, 1, if (left.value == right.value) 1 else 0, "equals")

  Violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v: CBLSIntVar, OldVal: Int, NewVal: Int) {
    Violation := (if (left.value == right.value) 1 else 0)
  }

  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override def violation = Violation
  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override def violation(v: Variable): CBLSIntVar = { if (left == v || right == v) Violation else 0 }

  override def checkInternals(c: Checker) {
    c.check(Violation.value == (if (left.value == right.value) 1 else 0),
      Some("Violation.value (" + Violation.value
        + ") == (if (left.value (" + left.value + ") == right.value (" + right.value + ")) 1 else 0)"))
  }
}

/**
 * constraints left == right
 * this is considered as a primitive constraint and used in the [[oscar.cbls.constraints.core.Constraint]]
 * class, so that it is part of the core instead of the library
 * @author renaud.delandtsheer@cetic.be
 */
case class EQ(left: CBLSIntVar, right: CBLSIntVar) extends Constraint {
  model = InvariantHelper.findModel(List(left,right))
  registerConstrainedVariables(left, right)
  finishInitialization()

  val Violation: CBLSIntVar = Abs(Minus(left, right))

  override def violation: CBLSIntVar = Violation
  override def violation(v: Variable): CBLSIntVar = { if (left == v || right == v) Violation else 0 }

  override def checkInternals(c: Checker) {
    val myViolation = abs(left.value - right.value)
    c.check(Violation.value == (if (left.value == right.value) 0 else myViolation),
      Some("Violation.value (" + Violation.value
        + ") == (if (left.value (" + left.value + ") == right.value (" + right.value
        + ")) 0 else " + myViolation + ")"))
  }
}
