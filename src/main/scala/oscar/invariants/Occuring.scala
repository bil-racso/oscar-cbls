/**
 * This file describes classes to use Event Programming in OscaR.
 *
 * This is of special interest for Discrete-Event Simulation, Agent-Based
 * simulation and invariants (functions maintained automatically from
 * changes to given variables).
 *
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

package oscar.invariants

import scala.collection.immutable._

/**
 * This trait represents a concept that regularly throws notification (of type A).
 * We can attach several functions that would be executed each time a notification is thrown.
 * @author smo
 *
 */
trait Occuring[A] {

  /**
   * Attach a new function that will be executed each time a notification is thrown.
   * When a notification occurs and this function is executed, this function will be
   * executed next time a notification is thrown iff it returns true at this call.
   */
  def foreach(f: A => Boolean): Reaction[A]

  def apply(f: A => Unit) = new ReactionDescription(this, { msg: A => f(msg); true })
  def    ~>(f: A => Unit) = this(f)

  /**
   * This methods returns an Occuring that will throw a notification each time this
   * throws a notification a such that filter(a)==true
   * @author smo
   *
   * @param <A>
   */
  def filter(filter: A => Boolean): Occuring[A] = new ConditionalOccuring(this, filter)
  def ===(a: A): Occuring[A] = filter(_ == a)
}

/**
 * A Reaction represents a given function that is executed when a notification is thrown
 * from a given occuring.
 *
 * The main interest is the ability to dispose this reaction, i.e. removing it entirely so
 * that f won't be called anymore for the future notification and the memory is freed.
 */
abstract class Reaction[A](val f: A => Boolean, occuring: Occuring[A]) extends Depending {
  def apply(msg: A) = {
    if (!f(msg)) {
      dispose()
      false
    } else
      true
  }
}

/**
 * This trait
 * @author smo
 *
 */
trait Depending {
  def dispose()
  def until(d: Occuring[_]) {
    once(d) { _ =>
      dispose()
    }
  }
}

/**
 * Simple class to implement Occuring.filter(f)
 */
class ConditionalOccuring[A](d: Occuring[A], f: A => Boolean) extends Occuring[A] {
  def foreach(f2: A => Boolean) = {
    for (msg <- d) {
      if (f(msg)) {
        f2(msg)
      } else {
        true
      }
    }
  }
}

/**
 * Reresents an occuring that stores all the function to be called at each notification
 * in a double linked list.
 *
 * This is the base class for Event. The methods emit(msg) is used by the subclasses to throw
 * notifications.
 */
trait BaseEvent[A] extends Occuring[A] {

  protected val dependants = new MyDLL[EventReaction]

  def emit(msg: A)

  class EventReaction(var event: BaseEvent[A], f: A => Boolean) extends Reaction[A](f, event) {
    val cf = dependants.add(this)
    def dispose() {
      cf silentlyRemove
    }
  }
  override def foreach(f: A => Boolean) = new EventReaction(this, f)
  def hanging = dependants.size
}

/**
 * These events notify all reactions listening to them. This is the default implementation.
 */
trait NotifyAllEvent[A] extends BaseEvent[A] {
  override def emit(msg: A) {
    for (d <- dependants) d(msg)
  }
  def toSignal(v: A) = {
    val res = new SignalAll(v)
    whenever(this) { res.emit(_) }
    res
  }
}

object Event {
  def apply[A]() = new NotifyAllEvent[A](){}
  def oneAtATime[A]() = new NotifyOneEvent[A](){}
}

/**
 * These events notify only the first reaction in the list. May be used to implements queues,
 * but this is still experimental. It is not straightforward to use them for correct implementation of queues.
 * Will likely be removeed in future versions.
 */
trait NotifyOneEvent[A] extends BaseEvent[A] {
  override def emit(a: A) {
    if (dependants.first != null) dependants.first.apply(a)
  }
}

/**
 * This class represents an Occuring that throws notifications each time any Occuring in a given list throws notifications.
 */
class OrEvent extends Reactive with NotifyAllEvent[Int] {
  var i = 0
  def |[A](eb: ReactionDescription[A]) = {
    val j = i
    dependsOn(eb.e) { msg: A =>
      eb.f(msg)
      this emit (j)
      dispose()
    }
    i += 1
    this
  }
}

/**
 * This class represents an Occuring that throws notifications each time all Occuring in a given list have thrown notifications.
 */
class AndEvent extends Reactive with NotifyAllEvent[Int] {
  var n = 0
  var tot = 0
  def &[A](eb: ReactionDescription[A]) = {
    val j = tot
    dependsOn(eb.occuring) { msg: A =>
      n += 1
      once(this) { _ => eb.f(msg) }
      if (n == tot) this emit (j)
      false
    }
    tot += 1
    this
  }
}

class ReactionDescription[A](val e: Occuring[A], val f: A => Boolean) {
  def occuring = e
  def |(eb: ReactionDescription[_]) = {
    val res = new OrEvent
    res | this | eb
    res
  }
  def &(eb: ReactionDescription[_]) = {
    val res = new AndEvent
    res & this & eb
    res
  }
  def post() = occuring.foreach(f)
}

object Signal {
  def apply[A](v: A) = new SignalAll[A](v)
}
/**
 * A Signal holds a value and throw a notification each time the value changes.
 */
class Signal[A](private var value: A) extends NotifyAllEvent[A] {
  override def emit(msg: A) {
    if (msg != value) {
      value = msg
      super.emit(msg)
    }
  }
  def apply() = value
  override def filter(f: A => Boolean) = new ConditionalOccuring[A](this, f) {
    // could be improved in the case the call to f(sig()) is false;
    // then we could avoid to create the Reaction
    override def foreach(f: A => Boolean) = {
      val r = super.foreach(f)
      if (f(value)) r(value)
      r
    }
  }

  //new EventFromNow(this, f)
}

//class SignalOne[A](value: A) extends Signal[A](value) with NotifyOneEvent[A] {
//  override def emit(msg: A) {
//    super[Signal].emit(msg)
//    super[NotifyOneEvent].emit(msg)
//  }
//}

class SignalAll[A](value: A) extends Signal[A](value) with NotifyAllEvent[A] {
  override def emit(msg: A) {
    super[Signal].emit(msg)
    //super[NotifyAllEvent].emit(msg)
  }
}

