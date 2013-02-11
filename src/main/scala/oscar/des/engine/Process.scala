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
import org.joda.time._

/**
 * Every simulated object taking part in the simulation should extend this class.
 * @author Pierre Schaus, Sebastien Mouthuy
 */
abstract class AbstractProcess[T](val name: String = "Process")(implicit m: Model[T]) {

  type State = Unit @cpsParam[SuspendableResult[T], SuspendableResult[T]]
  type susp = cpsParam[SuspendableResult[T], SuspendableResult[T]]
  def unit: Unit @susp = ()
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
   * Properly start the simulation of this process (method normally called by the engine, not the modeler).
   */
  def simulate(): Unit

  //def request(r: Resource): Unit @susp 

  def release(r: Resource) = {
    r.release()
  }

  def request(r: Resource) = {
    r.request[T]
  }
  //@noinline
  def waitDuring(d: Period): DateTime @susp = {
    waitFor[DateTime, T](m.clock === m.clock().plus(d))
  }
  def w[A](occ: Occuring[A]) = waitFor[A, T](occ)
  //def waitFor[B](occ: Occuring[B]) = oscar.invariants.waitFor(occ)

}

abstract class Process[T](name: String = "Process")(implicit m: Model[T]) extends AbstractProcess[T](name)(m) {

  /**
   * Entry point of the simulation for this process
   */
  def start(): T @susp

  /**
   * Properly start the simulation of this process (method normally called by the engine, not the modeler).
   */
  def simulate() {
    reset {
      End(start())
    }
  }
}

abstract class ProcessUnit[T](name: String = "Process")(implicit m: Model[T]) extends AbstractProcess[T](name)(m) {

  /**
   * Entry point of the simulation for this process
   */
  def start(): Unit @susp

  def simulate {
    reset {
      start()
      End
    }
  }
}

trait ProcessResult[T] {
  def +:(v: T): T
}

class DefaultResult extends ProcessResult[DefaultResult] {
  def +:(v: DefaultResult) = { new DefaultResult() }
}

abstract class ProcessWithStates[S, T](name: String = "Process", initState: S)(implicit m: Model[T]) extends Process[T](name)(m) {
  def exec(implicit state: S): T @susp
  def deepExec(state: S) = exec(state)
  def Iam(next: S)(implicit current: S) = deepExec(next)
  override def start() = deepExec(initState)
}
//
//abstract trait ProcessWithCostByState[S, T <: ProcessResult[T]] extends ProcessWithStates[S, T] {
//  def cost(state: S): T @susp
//  override def Iam(next: S)(implicit current: S) = cost(current).+:(deepExec(next))
//}

trait MonitorState[S, T] extends ProcessWithStates[S, T] {
  val entering = Event[S]()
  override def deepExec(state: S) = {
    entering.emit(state)
    super.deepExec(state)
  }
}

trait Precomputation[S, T] extends ProcessWithStates[S, T] {
  val results = mutable.HashMap[S, T]()

  def future(state: S) = {
    reset {
      val i = shift { k: (Int => SuspendableResult[T]) =>
        {
          k(0) match {
            case Suspend =>
              assert(false)
              Suspend
            case End =>
              assert(false)
              Suspend
            case EndResult(v) => {
              this(state) = v
              End(v)
            }
          }
        }
      }
      End(super.deepExec(state))
    }.get
  }

  override def deepExec(state: S): T @susp = {
//println( state.toString().substring(0,Math.min(45,state.toString().length())) )
    this(state) match {
      case None => {
        future(state)
        
      }
      case Some(v) => {
        //assert( future(state) == v )
        v
      }
    }
  }

  def apply(state: S) = results.get(state)
  def update(state: S, res: T) = {
//	 println( state.toString().substring(0,Math.min(45,state.toString().length())) + " *** " + res)
    assert(results.get(state) == None)
    results(state) = res
  }
    override def waitDuring(d: Period): DateTime @susp = {
      model.setTime(model.clock().plus(d))
      model.clock()
    }

}
