package oscar.modeling.algebra

import scala.collection.mutable.HashSet

/**
 * Number of different values in a given array of IntExpression
 */
case class NValues(val a: Array[IntExpression]) extends IntExpression {
  override def evaluate(): Int = a.map(_.evaluate()).toSet.size
  override def min: Int = 1
  override def max: Int = a.length
  override def values(): Iterable[Int] = min to max

  /**
   * Returns an iterable that contains all sub-expressions of this expression
   */
  override def subexpressions(): Iterable[IntExpression] = a

  /**
   * Apply a function on all sub-expressions of this expression and returns a new expression of the same type.
   * This function should return a value that is of the class as the object that was given to it.
   */
  override def mapSubexpressions(func: (IntExpression) => IntExpression): IntExpression = NValues(a.map(func))
}