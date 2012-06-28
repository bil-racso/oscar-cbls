package oscar.invariants

/* Own DoubleLinkedList
 * 
 */

import scala.collection.immutable._

class MyDLLElementContainer[C<:A,A](private var elem:C, var previous: MyDLLElementContainer[_<:A,A], var next: MyDLLElementContainer[_<:A,A]) {

  def foreach(f: (A) => Unit) {
    f(elem)
    if (next != null) next.foreach(f)
  }
  def apply = elem
}

class MyDLL[A] {

  var first: MyDLLElementContainer[_<:A,A] = null
  var  last: MyDLLElementContainer[_<:A,A] = null
  private var _size = 0
  def size = _size
  def add[C <: A](elem: C ) = {
    val ec = new MyDLLElementContainer[C,A](elem, last, null)
    if (last != null)
      last.next = ec
    else{
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
  def remove(ec: MyDLLElementContainer[_<:A,A]) {
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
  def foreach(f: (A) => Unit) { if (first != null) first.foreach(f) }

}
