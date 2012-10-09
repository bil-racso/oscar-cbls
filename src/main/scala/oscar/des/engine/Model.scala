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
import scala.collection.JavaConversions._
import oscar.invariants._

/**
 * This is the main engine of the simulation.
 * Every Process in the simulation should wait, require resource ... on an instance of this class.
 * @author pschaus
 */
class Model extends EsperanceSolver{

  val clock = new PQCounter[Long](0)

  private val processes = new LinkedList[Process]()

  def addProcess(p: Process) {
    processes.addLast(p)
  }

  def simulate(horizon: Long, verbose: Boolean = true) {
    // make all the process alive
    //reset{
    val it = processes.iterator
    while (it.hasNext) {
      it.next().simulate()
    }

    while (clock.nonEmpty && clock() <= horizon) {
      val e = clock.next

      if (verbose && e.time <= horizon) {
        val date = new java.util.Date()
        date.setTime(e.time.toLong)
        println("-----------> time: " + date)
      }
      if (clock() <= horizon) {
        e.process
      }
    }
    //}
  }
  @elidable(INFO) def print(s: String) {
    println(clock() + ": " + s)
  }
  def time(o: Any): Long = {
    clock()
  }
  def frequency[_](state: State[_]) = new Frequency(this, state)

  def waitDuration(duration: Long): Long @suspendable = {
    require(duration > 0)
    waitFor(clock === clock() + duration)

  }
  def waitDuration(duration: Int): Long @suspendable = {
	  require(duration > 0)
    waitFor(clock === clock() + duration)
  }

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
