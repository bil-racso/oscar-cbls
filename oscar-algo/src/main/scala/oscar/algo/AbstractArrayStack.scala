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
 *  
 */
abstract class AbstractArrayStack[T](initialSize: Int = 100) {

  private[this] var stack: Array[AnyRef] = Array.ofDim[AnyRef](initialSize)
  
  protected def augmentIndex()
  protected def decreaseIndex()
  protected def resetIndex()
  protected def index: Int
  

  /**
   *  Return the size of the stack
   *
   *  @return The size of the stack
   */
  @inline final def size: Int = index
  
  /**
   *  Return the size of the stack
   *
   *  @return The size of the stack
   */
  @inline final def length: Int = index

  /**
   *  Test if the stack is empty or not
   *
   *  @return `true` if the stack is empty, `false` otherwise
   */
  @inline final def isEmpty = index == 0

  /**
   *  Return the top element of the stack without removing it
   *
   *  Throws an exception if the stack is empty
   *
   *  @return The top element of the stack
   */
  @inline final def top: T = stack(index - 1).asInstanceOf[T]

  /**
   *  Return the last element of the stack in LIFO order
   *
   *  Throws an exception if the stack is empty
   *
   *  @return The last element of the stack in LIFO order
   */
  @inline final def last: T = {
    if (index == 0) sys.error("Stack empty")
    else stack(0).asInstanceOf[T]
  }

  /**
   *  Push an element onto the stack.
   *
   *  @param entry The element to push
   */
  @inline final def push(entry: T): Unit = {
    if (index == stack.length) growStack()
    stack(index) = entry.asInstanceOf[AnyRef] // boxing in case of primitive type
    augmentIndex()
  }
  
  /**
   *  Push an element onto the stack.
   *
   *  @param entry The element to push
   */
  @inline final def append(entry: T): Unit = push(entry)

  /**
   *  Pop the element on top of the stack
   *
   *  @return The element on top of the stack
   */
  @inline final def pop(): T = {
    if (index == 0) sys.error("Stack empty")
    decreaseIndex()
    stack(index).asInstanceOf[T]
  }

  /**
   *  Remove all the entries in the stack without removing references
   *  in the internal structure. This means that object cannot be
   *  garbage collected until references are overriden.
   */
  @inline final def clear(): Unit = resetIndex()

  /** Remove all the entries in the stack */
  @inline final def clearRefs(): Unit = {
    while (index > 0) {
      decreaseIndex()
      stack(index) = null
    }
  }
  
  @inline final def foreach[U](f: T => U): Unit = {
    var i = index
    while(i > 0) {
      i -= 1
      f(stack(i).asInstanceOf[T])
    }
  }
  
  @inline def apply(idx: Int): T = {
    if (idx >= index) throw new IndexOutOfBoundsException
    else stack(idx).asInstanceOf[T]
  }

  @inline def update(idx: Int, entry: T): Unit = {
    if (idx >= index) throw new IndexOutOfBoundsException
    else stack(idx) = entry.asInstanceOf[AnyRef]
  }
  
  // Double the size of the stack
  @inline private def growStack(): Unit = {
    val newStack = new Array[AnyRef](stack.length * 2)
    System.arraycopy(stack, 0, newStack, 0, stack.length)
    stack = newStack
  }
  
  
  import scala.reflect.ClassTag
  @inline final def toArray[T : ClassTag]: Array[T] = {
    val array = new Array[T](index)
    System.arraycopy(stack, 0, array, 0, index)
    array
  }
}
