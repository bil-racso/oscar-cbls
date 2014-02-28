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


package oscar.algo.reversible


/**
 * Creates a generic reversible pointer
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class ReversiblePointer[@specialized T](n: ReversibleContext, v: T) extends Reversible(n) {
  
  // Pointer to the object to trail
  private var pointer: T = v

  def setValue(value: T): Unit = {
    if (value != pointer) {
      trail()
      this.pointer = value
    }
  }

  /**
   * @param: value to assign
   */
  def value_= (value: T): Unit = setValue(value)
  
  /**
   * @param: value to assign
   */
  def := (value: T): Unit = setValue(value)
  
  /**
   * @return: current value
   */
  def value = getValue

  /**
   * Check if the pointer is different from null
   * @return true if the pointer is != null, false otherwise
   */
  def hasValue(): Boolean = pointer != null

  /**
   * @return the current pointer
   */
  def getValue(): T = pointer

  override def addOnTrail(): Unit = {
    node.getTrail().addEntry(this, pointer)
  }

  override def restore(value: Object): Unit = {
    pointer = value.asInstanceOf[T]
  }

  override def toString(): String = if (hasValue) s"$pointer" else ""
}

object ReversiblePointer {
  def apply[T](node: ReversibleContext, value: T): ReversiblePointer[T] = {
    new ReversiblePointer[T](node, value)
  }
}
