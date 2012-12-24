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
import scala.collection._
import scala.util.continuations._
import akka.util.Duration
import akka.util.FiniteDuration
//import akka.util.duration.
import oscar.invariants._

/**
 * Every simulated object taking part in the simulation should extend this class.
 * @author Pierre Schaus, Sebastien Mouthuy
 */
abstract class Process[T](name: String = "Process")(implicit m: Model[T]) {

  type State = Unit @cpsParam[Option[T], Option[T]]
  type susp = cpsParam[Option[T], Option[T]]
  //
  //  def suspend(): Unit @susp = {
  //    //		if (suspending) {
  //    //			//throw new RuntimeException("The process " + name + " is already suspending");
  //    //		}
  //    suspending = true
  //    shift { k: (Unit => Option[T]) =>
  //      suspended = { k() }
  //      None
  //    }
  //  }

  //  def resume() {
  //    //		if (!suspending){
  //    //			//throw new RuntimeException("The process " + name + " is not suspending");
  //    //		}
  //    suspending = false
  //    suspended
  //  }

  implicit val model = m
  m.addProcess(this)
  //  private var suspending = false
  //  private var suspended = {}

  /**
   * Entry point of the simulation for this process
   */
  def start(): T @cpsParam[Option[T], Option[T]]

  /**
   * Properly start the simulation of this process (method normally called by the engine, not the modeler).
   */
  def simulate() {
    reset {
      Some(start())
    }
  }

  //def request(r: Resource): Unit @susp 

  def release(r: Resource) = {
    r.release()
  }

  def request(r: Resource) = {
    r.request[T]
  }
  //@noinline
  def waitDuring(d: Long): Long @susp = {
    require(d > 0)
    waitFor[Long, T](m.clock === m.clock() + d)
  }
  def w[A](occ: Occuring[A]) = waitFor[A, T](occ)
  //def waitFor[B](occ: Occuring[B]) = oscar.invariants.waitFor(occ)

}

abstract class ProcessWithStates[T](name: String = "Process", initState: Any)(implicit m: Model[T]) extends Process[T](name)(m) {
  def exec(state: Any): T @susp
  def deepExec(state: Any) = exec(state)
  def Iam(state: Any) = deepExec(state)
  override def start() = deepExec(initState)
}

trait MonitorState[T] extends ProcessWithStates[T] {
  val entering = new Event[Any]
  override def deepExec(state: Any) = {
    entering.emit(state)
    super.deepExec(state)
  }
}

trait Precomputation[T] extends ProcessWithStates[T] {
  val results = mutable.HashMap[Any, T]()

  def future(state: Any) = {
    reset {
      val i = shift { k: (Int => Option[T]) =>
        {
          k(0) match {
            case None =>
              assert(false)
              None
            case Some(v) => {
              this(state) = v
              Some(v)
            }
          }
        }
      }
      Some(super.deepExec(state))
    }.get
  }

  override def deepExec(state: Any): T @susp = {

    this(state) match {
      case None => future(state)
      case Some(v) => {
        //assert( future(state) == v )
        v
      }
    }
  }

  def apply(state: Any) = results.get(state)
  def update(state: Any, res: T) = {
    assert(results.get(state) == None)
    results(state) = res
  }
  override def waitDuring(d: Long): Long @susp = {
    model.setTime(model.clock() + d)
    model.clock()
  }

}
