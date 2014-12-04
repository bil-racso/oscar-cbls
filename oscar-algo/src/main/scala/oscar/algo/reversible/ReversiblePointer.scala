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
class ReversiblePointer[@specialized T](val context: ReversibleContext, initialValue: T) {
  
  private var lastMagic: Long = -1L
  
  // Reference on the current value
  protected var pointer: T = initialValue
  
  @inline final def trail(): Unit = {
    val contextMagic = context.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      val entry = new ReversiblePointerTrailEntry[T](this, pointer)
      context.trail(entry)
    }
  }

  @inline final def setValue(value: T): Unit = {
    if (value != pointer) {
      trail()
      this.pointer = value
    }
  }

  /**
   * @param value to assign
   */
  @inline final def value_= (value: T): Unit = setValue(value)
  
  /**
   * @param value to assign
   */
  final def := (value: T): Unit = setValue(value)
  
  /**
   * @return current value
   */
  @inline final def value = pointer

  /**
   * Check if the pointer is different from null
   * @return true if the pointer is != null, false otherwise
   */
  @inline final def hasValue(): Boolean = pointer != null

  /**
   * @return the current pointer
   */
  @inline final def getValue(): T = pointer

  @inline final def restore(value: T): Unit = pointer = value

  override def toString(): String = if (hasValue) pointer.toString else ""
}

object ReversiblePointer {
  def apply[T](node: ReversibleContext, value: T): ReversiblePointer[T] = {
    new ReversiblePointer[T](node, value)
  }
}
