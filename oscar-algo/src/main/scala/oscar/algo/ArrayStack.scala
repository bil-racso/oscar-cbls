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


package oscar.algo

/**
 *  An array-based stack for objects.
 *  This means that primitive types are boxed.
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 *  @author Pierre Schaus pschaus@gmail.com
 */
class ArrayStack[T](initialSize: Int = 100) extends AbstractArrayStack[T] {

  private[this] var _index: Int = 0

  @inline final protected def augmentIndex() = _index += 1
  @inline final protected def decreaseIndex() = _index -= 1
  @inline final protected def resetIndex() = _index = 0
  @inline final protected def index: Int = _index
  
}
