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

import scala.util.Try

/**
 * Defines a type T that can be converted to and from a Double
 *
 * @author acrucifix acr@n-side.com
 */
trait ToFromDouble[T] {
  def toDouble(t: T): Double
  def fromDouble(d: Double): T
}

object ToFromDouble {
  implicit object _Double extends ToFromDouble[Double] {
    def toDouble(d: Double) = d
    def fromDouble(d: Double) = d
  }

  implicit object _Int extends ToFromDouble[Int] {
    def toDouble(i: Int) = i.toDouble
    def fromDouble(d: Double) = d.toInt
  }
}

/**
 * Represents a variable in a mathematical programming model.
 *
 * @author acrucifix acr@n-side.com
 */
abstract class AbstractMPVar[B](val name: String, val initialLowerBound: B, val initialUpperBound: B)(implicit solver: MPSolver[_], tfd: ToFromDouble[B]) extends oscar.algebra.Var {

  private var _lowerBound: B = initialLowerBound

  /**
   * Returns the lower bound.
   */
  def lowerBound: B = _lowerBound

  /**
   * Sets the lower bound to the given value.
   */
  def lowerBound_=(value: B) = {
    solver.updateLowerBound(name, tfd.toDouble(value))
    _lowerBound = value
  }

  private var _upperBound: B = initialUpperBound

  /**
   * Returns the upper bound.
   */
  def upperBound: B = _upperBound

  /**
   * Sets the upper bound to the given value.
   */
  def upperBound_=(value: B) = {
    solver.updateUpperBound(name, tfd.toDouble(value))
    _upperBound = value
  }

  /**
   * Returns the bounds of this variable (lower, upper)
   */
  def bounds: (B, B) = (lowerBound, upperBound)

  /**
   * Sets the bounds of this variable to the given value.
   *
   * @param lb = the new value for the lower bound
   * @param ub = the new value for the upper bound
   */
  def setBounds(lb: B, ub: B) = {
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
 * Represents a continuous variable
 *
 * @author acrucifix acr@n-side.com
 */
class FloatVar(name: String, initialLowerBound: Double = Double.NegativeInfinity, initialUpperBound: Double = Double.PositiveInfinity)(implicit solver: MPSolver[_]) extends AbstractMPVar[Double](name, initialLowerBound, initialUpperBound) {
  solver.add(this)
}

object FloatVar {
  def apply(name: String, lb: Double = Double.NegativeInfinity, ub: Double = Double.PositiveInfinity)(implicit solver: MPSolver[_]) = new FloatVar(name, lb, ub)
}

/**
 * Represents an integer variable
 *
 * @author acrucifix acr@n-side.com
 */
class IntVar(name: String, initialLowerBound: Int, initialUpperBound: Int)(implicit solver: MPSolver[_ <: MPSolverInterface with MIPSolverInterface]) extends AbstractMPVar[Int](name, initialLowerBound, initialUpperBound) {
  solver.add(this)
}

/**
 * Represents a binary (0-1) integer variable
 *
 * @author acrucifix acr@n-side.com
 */
class BinaryVar(name: String)(implicit solver: MPSolver[_ <: MPSolverInterface with MIPSolverInterface]) extends IntVar(name, 0, 1) {
  solver.add(this)
}
