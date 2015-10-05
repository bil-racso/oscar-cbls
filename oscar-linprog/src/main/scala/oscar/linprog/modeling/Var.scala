package oscar.linprog.modeling

import oscar.linprog.interface.{MIPSolverInterface, LPSolverInterface, MPSolverInterface}

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
abstract class AbstractMPVar[B](solver: MPSolverInterface, val index: Int, val name: String, val initialLowerBound: B, val initialUpperBound: B)(implicit tfd: ToFromDouble[B]) extends oscar.algebra.Var {

  private var _lowerBound: B = initialLowerBound

  /**
   * Returns the lower bound.
   */
  def lowerBound: B = _lowerBound

  /**
   * Sets the lower bound to the given value.
   */
  def lowerBound_=(value: B) = {
    solver.setVarLB(index, tfd.toDouble(value))
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
    solver.setVarUB(index, tfd.toDouble(value))
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
  override def value: Option[Double] =
    if(solver.hasSolution) Some(solver.getVarValue(index))
    else None
}

/**
 * Represents a continuous variable
 *
 * @author acrucifix acr@n-side.com
 */
class FloatVar(solver: MPSolverInterface with LPSolverInterface, index: Int, name: String, initialLowerBound: Double = Double.NegativeInfinity, initialUpperBound: Double = Double.PositiveInfinity) extends AbstractMPVar[Double](solver, index, name, initialLowerBound, initialUpperBound)

/**
 * Represents an integer variable
 *
 * @author acrucifix acr@n-side.com
 */
class IntVar(solver: MPSolverInterface with MIPSolverInterface, index: Int, name: String, initialLowerBound: Int, initialUpperBound: Int) extends AbstractMPVar[Int](solver, index, name, initialLowerBound, initialUpperBound)

/**
 * Represents a binary (0-1) integer variable
 *
 * @author acrucifix acr@n-side.com
 */
class BinaryVar(solver: MPSolverInterface with MIPSolverInterface, index: Int, name: String) extends IntVar(solver, index, name, 0, 1)
