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

import scala.collection.mutable._
import oscar.stochastic._
import annotation._
import annotation.elidable._
import scala.util.continuations._
import java.util.LinkedList
//import scala.collection.JavaConversions._
import oscar.invariants._
import org.joda.time.ReadableInstant
import org.scala_tools.time.Imports._

/**
 * This is the main engine of the simulation.
 * Every Process in the simulation should wait, require resource ... on an instance of this class.
 * @author pschaus
 */

class StochasticModel[+T] extends Model[T] with StochasticSolver[T]
class EsperanceModel[T <: Meanalizable[T]] extends Model[T] with EsperanceSolver[T]
class DeterministicModel[+T] extends Model[T] with DeterministicSolver[T]

/**
 * This class represents a signal whose value is equals to the number of months
 * last since the beginning of its creation.
 */
class MonthEvent(clock: Signal[DateTime]) extends Signal[Int](0) {

  reset {
    var m = 0
    while (true) {
      waitFor[DateTime, Unit](clock === clock().plusMonths(1).withDayOfMonth(1).withMillisOfDay(0))
      m += 1
      emit(m)
    }
    None
  }

}

abstract class Model[+T] extends DistrSolver[T] {

  //  
  implicit def comparable2ordered[A <: Comparable[A], B <: A](x: A): Ordered[B] =
    new Ordered[B] with Proxy {
      val self = x
      def compare(y: B): Int = { x.compareTo(y) }
    }
  implicit def DT2Ordered(dt: DateTime) = comparable2ordered[ReadableInstant, DateTime](dt)
  val n = DateTime.now
  //val b = comparable2ordered[ReadableInstant,DateTime](n)

  val clock = new PQCounter[DateTime](new DateTime(1970, 1, 1, 0, 0, 0, 0))
  val month = new MonthEvent(clock)

  private val processes = new LinkedList[Process[_]]()

  def addProcess(p: Process[_]) {
    processes.addLast(p)
  }

  def setTime(dateTime: DateTime) {
    clock.setTime(dateTime)
  }
  def setTime(d: String) { setTime(new DateTime(d)) }

  def simulate(horizon: DateTime, verbose: Boolean = true) {
    // make all the process alive
    //reset{
    val it = processes.iterator
    while (it.hasNext) {
      it.next().simulate()
    }

    while (clock.nonEmpty && clock() <= horizon) {
      val e = clock.next

      if (verbose && e.time <= horizon) {
        println("-----------> time: " + e.time)
      }
      if (clock() <= horizon) {
        e.process
      }
      println(clock.pq.size)
    }
    //}
  }
  @elidable(INFO) def print(s: String) {
    println(clock().toString() + ": " + s)
  }
  def time(o: Any) = clock()

  //def frequency(state: State[_]) = new Frequency(this, state)

  def waitDuration[T](duration: Period) = {
    waitFor(clock === (clock() + duration))

  }
  //  def waitDuration[T](duration: Int) = {
  //	  require(duration > 0)
  //    waitFor[Long,T](clock === clock() + duration)
  //  }

  //  def waitFor[A](ev: Signal[A], f: A => Boolean): Unit @suspendable = {
  //    if ( !f(ev())){ 
  //    var obs: Reaction[A] = null
  //    shift { k: (Unit => Unit) =>
  //      obs = when(ev) { (x: A) =>
  //        if (f(x)) {
  //          k()
  //        }
  //        true
  //      }
  //    }
  //    obs.dispose()
  //    }
  //  }

}

object Model {
  def main(args: Array[String]) {
    println(45)
  }
}
