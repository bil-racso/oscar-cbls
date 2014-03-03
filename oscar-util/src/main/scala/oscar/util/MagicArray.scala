package oscar.util

import scala.collection.mutable.IndexedSeq

/**
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class MagicArray(override val size: Int) extends IndexedSeq[Boolean] {
   
  private val array: Array[Int] = Array.fill(size)(Int.MinValue)
  private var magic = Int.MinValue + 1
  
  override val length: Int = size
  
  /** 
   *  Sets all the booleans to false in constant time.
   */
  def reset(): Unit = {
    if (magic != Int.MaxValue) magic += 1
    else {
      magic = Int.MinValue + 1
      var i = 0
      while (i < size) {
        array(i) = Int.MinValue
        i += 1
      }
    }
  }
  
  override def update(i: Int, b: Boolean): Unit = {
    if (b) array(i) = magic
    else array(i) = magic - 1
  }
  
  override def apply(i: Int): Boolean = array(i) == magic
  
  override def foreach[T](f: Boolean => T): Unit = {
    var i = 0
    while (i < size) {
      f(array(i) == magic)
      i += 1
    }
  }
}

object MagicArray {
  
  def fill(size: Int)(value: => Boolean): MagicArray = {
    val magic = new MagicArray(size)
    var i = 0
    while (i < size) { 
      magic(i) = value 
      i += 1
    }
    magic
  }
  
  def tabulate(size: Int)(f: Int => Boolean): MagicArray = {
    val magic = new MagicArray(size)
    var i = 0
    while (i < size) { 
      magic(i) = f(i)
      i += 1
    }
    magic
  }
  
  def apply(values: Boolean*): MagicArray = {
    var list = List.empty[Boolean]
    var size = 0
    values.foreach(v => {
      list = v :: list
      size += 1
    })
    val magic = new MagicArray(size)
    while (size > 0) {
      size -= 1
      magic(size) = list.head
      list = list.tail
    }
    magic
  }
}
