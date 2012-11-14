package oscar.cp.mem.pareto

import java.util.NoSuchElementException

class OrderedLinkedList[T] {

  private var fNode: LinkedNode[T] = null
  private var lNode: LinkedNode[T] = null

  private var _size = 0

  def size = _size
  def isEmpty = size == 0

  def first = fNode
  def last = lNode

  def first_=(x: LinkedNode[T]) { fNode = x }
  def last_=(x: LinkedNode[T]) { lNode = x }

  def insert(key: Int, value: T): LinkedNode[T] = {

    val node = new LinkedNode[T](key, value)

    if (isEmpty) {
      first = node
      last = node
    } else insert0(first, node)

    _size += 1
    node
  }

  def insert0(n: LinkedNode[T], newNode: LinkedNode[T]) {

    if (n.key > newNode.key) {

      if (n.isFirst) {
        first = newNode
        n.prev = newNode
        newNode.next = n
      } 
      else {
        newNode.prev = n.prev
        newNode.prev.next = newNode
        newNode.next = n
        n.prev = newNode
      }
    } 
    else if (n.isLast) {
      last = newNode
      newNode.prev = n
      n.next = newNode
    } 
    else insert0(n.next, newNode)
  }

  def remove(n: LinkedNode[T]) {

    if (isEmpty)
      throw new NoSuchElementException("This list is empty")
    else {
      if (n.isFirst) first = n.next else n.prev.next = n.next
      if (n.isLast) last = n.prev else n.next.prev = n.prev
    }

    _size -= 1
  }

  def clear() {
    _size = 0
    first = null
    last = null
  }
}