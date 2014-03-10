package oscar.util

import scala.collection.mutable.IndexedSeq

/**
 * A mutable IndexedSeq of Boolean which is able to sets all 
 * its booleans to false in constant time.
 * 
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class MagicArray(override val size: Int) extends IndexedSeq[Boolean] {
   
  private val array: Array[Int] = Array.fill(size)(Int.MinValue)
  private var magic = Int.MinValue + 1
  
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
  
  override val length: Int = size
  
  override def update(i: Int, b: Boolean): Unit = {
    if (b) array(i) = magic
    else array(i) = magic - 1
  }
  
  override def apply(i: Int): Boolean = array(i) == magic
  
  override def foreach[U](f: Boolean => U): Unit = {
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
    values.foreach(b => {
      list = b :: list
      size += 1
    })
    val magic = new MagicArray(size)
    list.foreach(b => {
      size -= 1
      magic(size) = b
    })
    magic
  }
}
