package oscar.cbls.search.algo

import scala.collection.Iterator
import scala.collection.immutable.SortedSet

/**
 * this proposes a set of methods to enable hot restart on iteration over an iterable.
 * it takes an Iterable[Int] and some pivot, and ensures that the iteration will explore
 * the values above the pivot first, in increasing order,
 * and the values below the pivot later, in increasing order as well.
*/
object HotRestart {
  def apply(it:Iterable[Int], pivot:Int):Iterable[Int] = new ShiftedIterable(it, pivot)

  def apply(r:Range, pivot:Int):Iterable[Int] =  new InstrumentedRange(r) startBy pivot
}


class ShiftedIterable(it:Iterable[Int], pivot:Int) extends Iterable[Int] {
  override def iterator: Iterator[Int] = {
    val (above, below) = it.partition(i => i > pivot)
    new ShiftedIterator(above, below)
  }

  class ShiftedIterator(first:Iterable[Int], var second:Iterable[Int]) extends Iterator[Int]{
    //TODO: this is awful: maybe the stuff is already sorted
    //TODO: we should perform a lazy sort since all the first might not be covered anyway
    var it:Iterator[Int] = first.toList.sorted.toIterator
    override def hasNext: Boolean = {
      if(it.hasNext) true
      else if (second == null) false
      else{
        it = second.toList.sorted.toIterator
        second = null
        it.hasNext
      }
    }

    override def next(): Int = it.next()
  }
}

class InstrumentedRange(r:Range){
  def startBy (start:Int)  =  new ShiftedRange(r.head, r.last,start:Int, r.step)
}

/**
 * this is an inclusive range.
 * @param start
 * @param end
 * @param startBy
 * @param step
 */
class ShiftedRange(val start:Int, val end:Int, val startBy:Int, val step:Int = 1) extends Iterable[Int]{
  if((start > startBy) || (startBy > end)) throw new Exception("ShiftedRange must contain startBy value ")
  if(step != 1) throw new Exception("only step of 1 is supported in ShirtedRange")

  //include the at Value
  private def unfold(at:Int):List[Int] = {
    if(at == end){
      unfold (start)
    }else if(getNextValue(at) == startBy){
      List(at)
    }else{
      at :: unfold(at+1)
    }
  }

  def getNextValue(a:Int) = {
    if(a == end) start
    else a+1
  }

  override def iterator: Iterator[Int] = new ShiftedRangeIterator(this)

  override def toList: List[Int] = unfold(startBy)

  override def toArray[B >: Int](implicit evidence$1: scala.reflect.ClassTag[B]): Array[B] = toList.toArray

  override def toString(): String = "ShiftedRange(" + toList + ")"


  class ShiftedRangeIterator(val s:ShiftedRange) extends Iterator[Int]{
    var currentValue = s.startBy

    def hasNext: Boolean = (s.getNextValue(currentValue) != s.startBy)

    def next(): Int = {
      val tmp = currentValue
      currentValue = s.getNextValue(currentValue)
      tmp
    }
  }
}