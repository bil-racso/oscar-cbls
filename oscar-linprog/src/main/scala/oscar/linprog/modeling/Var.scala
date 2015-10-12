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

package oscar.linprog.modeling

import oscar.linprog.interface.{MIPSolverInterface, MPSolverInterface}

/**
 * Describes the type of an MPVar: [[Continuous]], [[Integer]] or [[Binary]]
 *
 * @author acrucifix acr@n-side.com
 */
sealed abstract class MPVarType(name: String) {
  def createVar(variable: MPVar)(implicit solver: MPSolver[_])

  override def toString = name
}

/**
 * Variables of this type can take values in the real domain.
 *
 * @author acrucifix acr@n-side.com
 */
case object Continuous extends MPVarType("continuous") {
  def createVar(variable: MPVar)(implicit solver: MPSolver[_]) =
    solver.addFloatVar(variable)
}

/**
 * Variables of this type can take values in the integer domain.
 *
 * @author acrucifix acr@n-side.com
 */
case object Integer extends MPVarType("integer") {
  def createVar(variable: MPVar)(implicit solver: MPSolver[_]) =
    solver.asInstanceOf[MPSolver[_ <: MIPSolverInterface]].addIntVar(variable)
}

/**
 * Variables of this type can take values in the binary (0-1) domain.
 *
 * @author acrucifix acr@n-side.com
 */
case object Binary extends MPVarType("binary") {
  def createVar(variable: MPVar)(implicit solver: MPSolver[_]) =
    solver.asInstanceOf[MPSolver[_ <: MIPSolverInterface]].addBinaryVar(variable)
}

/**
 * Represents a variable in a mathematical programming model.
 *
 * @author acrucifix acr@n-side.com
 */
class MPVar private (var varType: MPVarType, val name: String, val initialLowerBound: Double, val initialUpperBound: Double)(implicit solver: MPSolver[_]) extends oscar.algebra.Var {
  private var _lowerBound = initialLowerBound
  private var _upperBound = initialUpperBound

  varType.createVar(this)

  /**
   * Returns the lower bound.
   */
  def lowerBound = _lowerBound

  /**
   * Sets the lower bound to the given value.
   */
  def lowerBound_=(value: Double) = {
    solver.updateLowerBound(name, value)
    _lowerBound = value
  }

  /**
   * Returns the upper bound.
   */
  def upperBound = _upperBound

  /**
   * Sets the upper bound to the given value.
   */
  def upperBound_=(value: Double) = {
    solver.updateUpperBound(name, value)
    _upperBound = value
  }

  /**
   * Returns the bounds of this variable (lower, upper)
   */
  def bounds = (lowerBound, upperBound)

  /**
   * Sets the bounds of this variable to the given value.
   *
   * @param lb = the new value for the lower bound
   * @param ub = the new value for the upper bound
   */
  def setBounds(lb: Double, ub: Double) = {
    lowerBound = lb
    upperBound = ub
  }

  /**
   * Resets the bounds of this variable to their initial value
   */
  def resetBounds() = {
    lowerBound = initialLowerBound
    upperBound = initialUpperBound
  }

  /**
   * Returns the value of this variable in the solution found by the solver if any
   */
  override def value: Option[Double] = solver.value(name).toOption
}

/**
 * Creates variables
 *
 * @author acrucifix acr@n-side.com
 */
object MPVar {
  def float(name: String, lb: Double = Double.NegativeInfinity, ub: Double = Double.PositiveInfinity)(implicit solver: MPSolver[_ <: MPSolverInterface]): MPVar =
    new MPVar(Continuous, name, lb, ub)

  def int(name: String, from: Int, to: Int)(implicit solver: MPSolver[_ <: MPSolverInterface with MIPSolverInterface]): MPVar =
    new MPVar(Integer, name, from, to)

  def binary(name: String)(implicit solver: MPSolver[_ <: MPSolverInterface with MIPSolverInterface]): MPVar =
    new MPVar(Binary, name, 0.0, 1.0)
}

/**
 * Creates continuous variables
 *
 * @author acrucifix acr@n-side.com
 */
object MPFloatVar {
  def apply(name: String, lb: Double = Double.NegativeInfinity, ub: Double = Double.PositiveInfinity)(implicit solver: MPSolver[_ <: MPSolverInterface]): MPVar =
    MPVar.float(name, lb, ub)
}

/**
 * Creates integer variables
 *
 * @author acrucifix acr@n-side.com
 */
object MPIntVar {
  def apply(name: String, from: Int, to: Int)(implicit solver: MPSolver[_ <: MPSolverInterface with MIPSolverInterface]): MPVar =
    MPVar.int(name, from, to)
  def apply(name: String, range: Range)(implicit solver: MPSolver[_ <: MPSolverInterface with MIPSolverInterface]): MPVar =
    apply(name, range.min, range.max)
}

/**
 * Creates binary (0-1) integer variables
 *
 * @author acrucifix acr@n-side.com
 */
object MPBinaryVar {
  def apply(name: String)(implicit solver: MPSolver[_ <: MPSolverInterface with MIPSolverInterface]): MPVar =
    MPVar.binary(name)
}
