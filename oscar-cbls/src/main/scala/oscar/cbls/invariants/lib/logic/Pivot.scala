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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 *            Yoann Guyot
 ******************************************************************************/


package oscar.cbls.invariants.lib.logic

import collection.immutable.SortedSet
import collection.mutable.Queue
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.algo.heap.{ BinomialHeap, BinomialHeapWithMove }
import oscar.cbls.invariants.core.propagation.Checker

/**
 * {i in index of values | values[i] <= boundary}
 * It is based on two heap data structure, hence updates are log(n) and all updates are allowed
 * @param values an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]]
 * @param boundary the boundary for comparison
 */
case class SelectLEHeapHeap(values: Array[CBLSIntVar], boundary: CBLSIntVar) extends SetInvariant {
  var output: CBLSSetVar = null

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v), v)
  registerStaticAndDynamicDependency(boundary)
  finishInitialization()

  def myMin = values.indices.start
  def myMax = values.indices.end

  val HeapAbove: BinomialHeapWithMove[Int] = new BinomialHeapWithMove((i: Int) => values(i).value, values.size)
  val HeapBelowOrEqual: BinomialHeapWithMove[Int] = new BinomialHeapWithMove((i: Int) => -(values(i).value), values.size)

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)
    output := SortedSet.empty[Int]
    for(v <- values.indices){
      if(values(v).value <= boundary.value){
        HeapBelowOrEqual.insert(v)
        output.insertValue(v)
      } else {
        HeapAbove.insert(v)
      }
    }
  }

  //pomper des elements de Above et les mettre dans Below et dans output
  @inline
  def TransferToBelow() {
    while (!HeapAbove.isEmpty && values(HeapAbove.getFirst).value <= boundary.value) {
      val v = HeapAbove.removeFirst()
      HeapBelowOrEqual.insert(v)
      output.insertValue(v)
    }
  }

  //pomper des elements de Above et les mettre dans Below et dans output
  @inline
  def TransferToAbove() {
    //pomper des elements de beloworequal et les mettre dans above
    while (!HeapBelowOrEqual.isEmpty && values(HeapBelowOrEqual.getFirst).value > boundary.value) {
      val v = HeapBelowOrEqual.removeFirst()
      HeapAbove.insert(v)
      output.deleteValue(v)
    }
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, i: Int, OldVal: Int, NewVal: Int) {
    if (v == boundary) {
      //c'est le boundary
      if (NewVal > OldVal) {
        TransferToBelow()
      } else { TransferToAbove() }
    } else {
      if (OldVal <= boundary.value) {
        //il est dans BelowOrEqual
        HeapBelowOrEqual.notifyChange(i)
        TransferToAbove()
      } else {
        HeapAbove.notifyChange(i)
        TransferToBelow()
      }
    }
  }

  override def checkInternals(c: Checker) {
    for (v <- output.value) {
      c.check(values(v).value <= boundary.value,
        Some("values(" + v + ").value (" + values(v).value
          + ") <= boundary.value (" + boundary.value + ")"))
    }
    var count: Int = 0
    for (v <- values) {
      if (v.value <= boundary.value)
        count += 1
    }
    c.check(count == output.value.size, Some("count (" + count
      + ") == output.value.size (" + output.value.size + ")"))
    c.check(HeapAbove.size + HeapBelowOrEqual.size == values.size,
      Some("HeapAbove.size + HeapBelowOrEqual.size ("
        + HeapAbove.size + "+" + HeapBelowOrEqual.size
        + ") == values.size (" + values.size + ")"))
  }
}

/**
 * {i \in index of values | values[i] <= boundary}
 * It is based on a queue for the values above the boundary, hence all updates must be accepted by this scheme:
 * - SelectLESetQueue does not allow boundary to decrease
 * - SelectLESetQueue does not allow elements above boundary to change
 * - SelectLESetQueue requires latest variables passing above boundary to be the biggest one
 * @param values: an array of intvar
 * @param boundary: the boundary for comparison
 */
case class SelectLESetQueue(values: Array[CBLSIntVar], boundary: CBLSIntVar) extends SetInvariant {
  var output: CBLSSetVar = null

  def myMin = values.indices.start
  def myMax = values.indices.end

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v), v)
  registerStaticAndDynamicDependency(boundary)
  finishInitialization()

  val QueueAbove: Queue[Int] = new Queue[Int]

  override def setOutputVar(v: CBLSSetVar) {
    output = v
    output.setDefiningInvariant(this)
    output := SortedSet.empty[Int]
    val HeapAbove: BinomialHeap[Int] = new BinomialHeap((i: Int) => values(i).value, values.size)
    for (v <- values.indices) {
      if (values(v).value <= boundary.value) {
        output.insertValue(v)
      } else {
        HeapAbove.insert(v)
      }
    }
    while (!HeapAbove.isEmpty) {
      QueueAbove.enqueue(HeapAbove.popFirst())
    }
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, index: Int, OldVal: Int, NewVal: Int) {
    if (v == boundary) {
      //c'est le boundary
      assert(NewVal > OldVal, "SelectLESetQueue does not allow boundary to decrease")
      while (!QueueAbove.isEmpty && values(QueueAbove.head).value <= boundary.value) {
        val v = QueueAbove.dequeue()
        output.insertValue(v)
      }
    } else { //il est dans BelowOrEqual
      //     println("SelectLEnotify " + v + " index: " + index +  " OldVal: " + OldVal + " NewVal: " + NewVal + " boundary: " + boundary + " output " + output)
      assert(OldVal <= boundary.value, "SelectLESetQueue does not allow elements above boundary to change")
      assert(QueueAbove.isEmpty || values(QueueAbove.last).value <= NewVal, "SelectLESetQueue requires latest variables passing above boundary to be the biggest one")
      QueueAbove.enqueue(index)
      output.deleteValue(index)
    }
  }

  override def checkInternals(c: Checker) {
    var count: Int = 0
    for (i <- values.indices) {
      if (values(i).value <= boundary.value) {
        c.check(output.value.contains(i), Some("output.value.contains(" + i + ")"))
        count += 1
      }
    }
    c.check(output.value.size == count,
      Some("output.value.size (" + output.value.size
          + ") == count (" + count + ")"))
  }
}
