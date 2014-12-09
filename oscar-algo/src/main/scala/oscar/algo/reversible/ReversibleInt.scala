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
package oscar.algo.reversible;

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class ReversibleInt(node: ReversibleContext, value: Int) extends ReversiblePointer[Int](node, value) {

  /** Increments the reversible integer by one */
  @inline final def incr(): Int = {
    trail()
    pointer += 1
    pointer
  }

  /** Decrements the reversible integer by one */
  @inline final def decr(): Int = {
    trail()
    pointer -= 1
    pointer
  }

  /** Increments the reversible integer by i */
  @inline final def +=(i: Int): Int = {
    trail()
    pointer += i
    pointer
  }

  /** Decrements the reversible integer by i */
  @inline final def -=(i: Int): Int = {
    trail()
    pointer -= i
    pointer
  }
}

object ReversibleInt {
  def apply(value: Int)(implicit context: ReversibleContext) = new ReversibleInt(context, value)
}
