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

package oscar.examples.des

import oscar.des.engine._
import oscar.des.engine
import oscar.invariants._
import scala.util.continuations._
import JSci.maths.statistics._

//import org.scala_tools.time.Imports._
//import oscar.util.date._

/**
 * A garage owner has installed an automatic car wash that services cars one at a time.
 * When a car arrives, it goes straight into the car wash if this is idle; otherwise, it must wait in a queue.
 * The car washer starts his day in a tearoom and return there each time he has no work to do.
 * As long as cars are waiting, the car wash is in continuous operation serving on a first-come, first-served basis.
 * All cars that have arrived before the garage closes down are washed.
 * Each service takes exactly 10 minutes.
 * The average time between car arrivals has been estimated at 11 minutes.
 * The garage owner is interested in predicting the maximum queue length and average waiting time if he installs
 * one more car wash and employs one more car washer.
 *
 * @author Sebastien Mouthuy
 */

// seb, this example needs to be fixed, Loggin is not available
/*
object CarWash extends Learning {
  val efficiency = learnNumber[Double]
  def main(args: Array[String]) {

    val nbIter = 1

    for (i <- 1 to nbIter) {
      sim()
    }

    //println(obs.timeWorking)
    println("Efficiency of the washer: " + efficiency.tot / nbIter)
  }
  def sim() {

    implicit val m = new StochasticModel[Unit]

    val beginTime = 1 january 2013 at 8 h 00
    val endTime = beginTime.plusHours(8)

    m.setTime(beginTime)
    // one day = 8 hours
    val endOfDay = m.clock === endTime

    class CarWash(implicit val m: Model[Unit]) extends Process[Unit]("CarWash") {
      var nbCarsWashed = 0
      val queue = new SimQueue()
      once(endOfDay) { _ => queue.close() }

      val isOpened = new Var[Boolean](false)

      def request() = {
        if (queue.enter[Unit]) {
          waitFor(isOpened === true)
          waitDuring(10.minute)
          nbCarsWashed += 1
          true
        } else false
      }
      def start = {}
      def release() { queue.leave }

      def open() {
        isOpened := true
      }

      def close() { isOpened := false }

    } // end of CarWash

    trait CarWasherState
    case class AtTeaRoom(i: Int) extends CarWasherState
    case class AtCarWash(i: Int) extends CarWasherState
    case class AtHome() extends CarWasherState
    case class ToCarWash() extends CarWasherState

    class CarWasher(carWash: CarWash)(implicit m: Model[Unit]) extends ProcessWithStates[CarWasherState, Unit]("Washer", ToCarWash())(m) with MonitorState[CarWasherState, Unit] {

      val getSick = new Flip(0.05)

      var eod = false
      once(endOfDay) { _ => eod = true }

      val beenAtTeaRoom = Event[Interval]

      def exec(implicit state: CarWasherState) = {
        state match {
          case ToCarWash() => {
            carWash.queue.open()
            Iam(AtTeaRoom(0))
          }
          case AtTeaRoom(i) => {
            val t0 = m.clock()
            m print ("washer at tea room")
            println(carWash.queue.isEmpty())
            val opt = waitFor(carWash.queue.isEmpty === false | endOfDay)
            beenAtTeaRoom.emit(t0 to m.clock())
            if (opt == 1) this.Iam(AtHome())
            else Iam(AtCarWash(i + 1))
          }
          case AtCarWash(i) => {
            m print ("washer at car wash")
            val n0 = carWash.nbCarsWashed
            carWash.open()
            waitFor(carWash.queue.isEmpty === true)
            carWash.close()
            if (getSick(m)) Iam(AtHome())
            else if (eod) Iam(AtHome())
            else Iam(AtTeaRoom(i))
          }
          case AtHome() => {
            m print ("...finally")
          }
          case _ => {
            println(state)
            throw new RuntimeException()
          }
        }
      }
    }

    class CarWashVisu(carWash: CarWash){
      // Create all your swing abjects
      
      whenever (carWash isOpened){_ match{
        case true => // update your visu because the CarWash is open
        case false => // update your visu because the CarWash is closed
      }}
    }
    class CarWasherVisu(washer: CarWasher) {

      // Create all your Swing panels,...

      whenever(washer.entering) {
        _ match {
          case AtTeaRoom(_) => // Update your swing object so that it reflects that the washer is having a coffee
          case AtHome() => // ...
        }
      }

    }

    class Car(m: Model[Unit], carWash: CarWash, id: String) extends Process[Unit](id)(m) {
      def start() = {
        m print (id + ": arrives")
        if (carWash.request()) carWash.release()
      }
    }

    class CarWashObserver(cw: CarWash) {
      var lastTime = cw.m.clock()
      var timeWorking: Long = 0
      whenever(cw.isOpened === false) {
        timeWorking += (lastTime to cw.m.clock()).millis
        lastTime = cw.m.clock()
      }
    }
    class CarWasherObserver(w: CarWasher) {
      var lastEntered: DateTime = now
      var totalTimeSpentAtTeaRoom: Long = 0
      whenever(w.entering) { s =>
        s match {
          case _            => totalTimeSpentAtTeaRoom += (lastEntered to w.model.clock()).millis
          case AtTeaRoom(_) => lastEntered = w.model.clock()
        }
        println(s)
      }

    }

    val carWash = new CarWash
    val carWasher = new CarWasher(carWash)

    var i = 0

    val carGenerator = Generator(new ExponentialD(1.0 / 20.minute.millis)) {
      val c = new Car(m, carWash, "car:" + i)
      i += 1
      c.simulate()
      true
    }

    val obs = new CarWashObserver(carWash)
    val obs2 = new CarWasherObserver(carWasher)
    m.simulate(endTime.plusHours(2), false)

    efficiency observe ((8.hour.millis - obs2.totalTimeSpentAtTeaRoom) * 1.0 / 8.hour.millis)
    //for ( en <- carWasher.results){println(en)}
  }

}
*/
