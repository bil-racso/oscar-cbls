/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

package oscar.invariants

/* Own DoubleLinkedList
 * 
 */

import scala.collection.immutable._

class MyDLL[A] {

  class MyDLLElementContainer[C <: A](private var elem: C, var previous: MyDLLElementContainer[_ <: A], var next: MyDLLElementContainer[_ <: A]) {

    var list = MyDLL.this
    def apply = elem
    def silentlyRemove {
      if (list != null) {
        MyDLL.this.remove(this)
        list == null
      }
    }
    def remove {
      require(list != null, "This element has already been removed")
      silentlyRemove
    }
    
    override def toString() : String = elem.toString()
  }

  var first: MyDLLElementContainer[_ <: A] = null
  var last: MyDLLElementContainer[_ <: A] = null
  private var _size = 0
  def size = _size
  def add[C <: A](elem: C) = {
    val ec = new MyDLLElementContainer[C](elem, last, null)
    if (last != null)
      last.next = ec
    else {
      first = ec
    }
    last = ec
    _size += 1
    ec
  }
  //  def addElement(ec: MyDLLElementContainer[_<:A,A]) = {
  //    ec.previous = null
  //    ec.next = first
  //    if (first != null)
  //      first.previous = ec
  //    first = ec
  //    ec
  //  }
  private def remove(ec: MyDLLElementContainer[_ <: A]) {
    if (ec.previous == null)
      first = ec.next
    else
      ec.previous.next = ec.next

    if (ec.next == null)
      last = ec.previous
    else
      ec.next.previous = ec.previous
    _size -= 1
  }
  def foreach(f: (A) => Unit) {
    var current = first
    while (current != null) {
      f(current.apply)
      current = current.next
    }
  }
  
  override def toString() : String = {
    var current = first
    var string = ""
    while (current != null) {
      string += current.toString() + " , "
      current = current.next
    }
    string
  }
}
