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

package oscar.des.engine

import oscar.invariants._
//import scala.collection.mutable._
import java.util.PriorityQueue
import scala.util.continuations._

/**
 * This class represents a counter which holds the current value. It uses a priority queue indexed by value
 * to store any function that should be executed when the counter reaches a certain value. This makes
 * filtering on this signal very efficient. It offers methods
 * to retrieve the next value having a function "attached" to it.
 *
 * This class is particularly well suited to implements clocks.
 *
 * param v: initial value of the counter
 *
 * @author Sebastien Mouthuy
 */
class PQCounter[A <% Ordered[A]](v: A) extends Signal[A](v) {

  class PQEventBlock(ev: WaitEvent[A]) extends Reaction[A]({ _ => false }, PQCounter.this) {
    def dispose() {
      removeEvent(ev)
    }
  }

  val pq = new PriorityQueue[WaitEvent[A]]

  def addEvent(ev: WaitEvent[A]) {
    require(ev.time >= this())
    pq.add(ev)
  }

  /**
   * sets the current value of the counter
   */
  def update(t: A) = setTime(t)
  def setTime(t: A) {
    require(t >= this())
    require(pq.isEmpty() || t <= nextTime)

    if (!pq.isEmpty() && t == nextTime) {
      generateNext
    } else {
      emit(t)
    }
  }
  def currentTime = this()
  /**
   * returns the next value for which a reaction has been set
   */
  def nextTime = pq.peek().time

  override def ===(i: A) = {
    new Occuring[A] {
      override def foreach(f2: A => Boolean) = {
        assert(currentTime <= i, s"Trying to define an event in the past. currentTime is $currentTime and eventTime is $i")
        val a = new WaitEvent[A](i, f2)
        PQCounter.this addEvent (a)
        new PQEventBlock(a)
      }
    }
  }
  def >=(i: A) = {
    new Occuring[A] {
      override def foreach(f2: A => Boolean) = {
        val t = if (currentTime > i) currentTime else i
        val a = new WaitEvent[A](t, f2)
        PQCounter.this addEvent (a)
        new PQEventBlock(a)
      }
    }
  }
  override def hanging = pq.size()
  def nonEmpty = !pq.isEmpty()

  /**
   * Executes all the reactions holding on this counter.
   *
   * It loops by setting the next value such that a reaction holds,
   * executing the given function, until there is no more reaction to process.
   * Reaction may be added to the counter during this process (if they are strictly greater than
   * the current value).
   */
  def generate() {
    while (nonEmpty) {
      generateNext()
    }
  }

  private def generateNext() {
    val ev = next
    println("counter " + ev.time)
    ev.process
  }
  def next = {
    val res = pq.poll()
    this emit res.time
    res
  }

  def dispose {
    pq.clear()
  }

  private def removeEvent(ev: WaitEvent[A]) { pq.remove(ev) }
}


object Counter {

  def pq[A <% Ordered[A]](v: A) = new PQCounter(v)

  def main(args: Array[String]) {

    val x = new IncrementalVar(5)
    val y = new IncrementalVar(8)

    val c = new PQCounter(0)

    reset {
      oscar.invariants.hangUntil[Int, Unit](x)
      println("500")
      End
    }

    whenever(c === 5) { w: Int =>
      println("super")
    }
    println("here")
    x := 500

    c.generate()
    None
  }
}
