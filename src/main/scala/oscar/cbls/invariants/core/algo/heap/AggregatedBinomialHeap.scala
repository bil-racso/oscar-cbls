package oscar.cbls.invariants.core.algo.heap

import collection.Iterator

/*******************************************************************************
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
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

/**This class is a faster version of a heap, where several items stored in it have same index.
 * The heap is actually made of an array, storing lists containing items with same position.
 * A binomial heap is maintained to record the lowest position in the heap.
 * This is more efficient if it often occurs that elements have the same position.
 * keys is assumed to start at zero.
 */
class AggregatedBinomialHeap[T](GetKey:T => Int,MaxPosition:Int) extends AbstractHeap[T] {

  val b= new BinomialHeap[Int](a => a, MaxPosition)

  val a:Array[List[T]] = Array.tabulate (MaxPosition)(_ => List.empty[T])

  private var msize:Int = 0

  /**makes the datastruct empty*/
  override def dropAll{
    for (i <- b) a(i) = List.empty[T]
    msize = 0
    b.dropAll
  }

  override def insert(elem:T){
    val position = GetKey(elem)
    val otherWithSamePosition = a(position)
    if (otherWithSamePosition.isEmpty){
      a(position) = List(elem)
      b.insert(position)
    }else{
      //this is the desired branch, as it is O(1)
      a(position) = elem :: otherWithSamePosition
    }
    msize+=1
  }

  override def getFirsts:List[T] = {
    assert(!isEmpty)
    val toreturn = a(b.getFirst)
    assert(!toreturn.isEmpty)
    toreturn
  }

  override def popFirsts:List[T] = {
    assert(!isEmpty)
    val toreturn = a(b.popFirst())
    assert(!toreturn.isEmpty)
    toreturn
  }

  override def isEmpty:Boolean = (msize == 0)
  override def size = msize

  override def getFirst: T = getFirsts.head

  override def popFirst(): T = {
    val position = b.getFirst
    val liste = a(position)
    val toreturn = liste.head
    a(position) = liste.tail
    if (liste.tail.isEmpty){
      b.popFirst()
    }
    msize -=1
    toreturn
  }

  override def iterator: Iterator[T] = {
    var acc:List[T] = List.empty
    for (position <- b){
      for (item <- a(position)){
        acc = item :: acc
      }
    }
    acc.iterator
  }
}
