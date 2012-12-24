///**
// * *****************************************************************************
// * This file is part of OscaR (Scala in OR).
// *
// * OscaR is free software: you can redistribute it and/or modify
// * it under the terms of the GNU General Public License as published by
// * the Free Software Foundation, either version 2.1 of the License, or
// * (at your option) any later version.
// *
// * OscaR is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License along with OscaR.
// * If not, see http://www.gnu.org/licenses/gpl-3.0.html
// * ****************************************************************************
// */
//
//package oscar.examples.des
//
//import oscar.des.engine._
//import oscar.des.engine
//import oscar.invariants._
//import scala.util.continuations._
//import JSci.maths.statistics._
//import oscar.stochastic._
//
//import akka.util.Duration._
//import akka.util.duration._
//
///**
// * A garage owner has installed an automatic car wash that services cars one at a time.
// * When a car arrives, it goes straight into the car wash if this is idle; otherwise, it must wait in a queue.
// * The car washer starts his day in a tearoom and return there each time he has no work to do.
// * As long as cars are waiting, the car wash is in continuous operation serving on a first-come, first-served basis.
// * All cars that have arrived before the garage closes down are washed.
// * Each service takes exactly 10 minutes.
// * The average time between car arrivals has been estimated at 11 minutes.
// * The garage owner is interested in predicting the maximum queue length and average waiting time if he installs
// * one more car wash and employs one more car washer.
// *
// * @author Sebastien Mouthuy
// */
//object CarWash {
//  val efficiency = new LearnedNumerical[Double]()(DoubleOp)
//  def main(args: Array[String]) {
//
//    val nbIter = 1
//    
//    for (i <- 1 to nbIter) {
//      sim()
//    }
//
//    //println(obs.timeWorking)
//    println("Efficiency of the washer: " + efficiency.tot / nbIter)
//  }
//  def sim() {
//
//    implicit val m = new StochasticModel[Nothing]
//
//    // one day = 8 hours
//    val endOfDay = m.clock === 8.hour
//
//    
//    class CarWash(val m: Model[Unit]) {
//    	var nbCarsWashed = 0
//      val queue = new SimQueue()
//      once(endOfDay) { _ => queue.close() }
//
//      val isOpened = new Var[Boolean](false)
//
//      def request[T](): Boolean @cpsParam[Option[T],Option[T]] = {
//        val res = if (queue.enter[T]) {
//          waitFor[Boolean,T](isOpened === true)
//          waitFor[Long,T](m.clock === m.clock() + 10.minute)
//          nbCarsWashed += 1
//          true
//        }else false
//        res
//      }
//
//      def release() { queue.leave }
//
//      def open() {
//        isOpened := true
//      }
//
//      def close() { isOpened := false }
//
//    } // end of CarWash
//
//    case class AtTeaRoom(i: Int)
//    case class AtCarWash(i: Int)
//    case class AtHome()
//    case class ToCarWash()
//    class CarWasher(m: Model[Double], carWash: CarWash) extends ProcessWithStates[Double]("Washer", ToCarWash)(m) with MonitorState[Double] {
//
//      val getSick = new Flip(0.05)
//      
//      var eod = false
//      once(endOfDay) { _ => eod = true }
//
//      val beenAtTeaRoom = new Event[Long]
//
//      def exec(state: Any) = {
//        state match {
//          case ToCarWash => {
//            carWash.queue.open()
//            Iam(AtTeaRoom(0))
//          }
//          case AtTeaRoom(i) => {
//            val t0 = m.clock()
//            m print ("washer at tea room")
//            println(carWash.queue.isEmpty())
//            val opt = w(carWash.queue.isEmpty === false | endOfDay)
//            beenAtTeaRoom.emit(m.clock() - t0)
//            if (opt == 1) this.Iam(AtHome)
//            else          Iam(AtCarWash(i+1))
//          }
//          case AtCarWash(i) => {
//            m print ("washer at car wash")
//            val n0 = carWash.nbCarsWashed
//            carWash.open()
//            w(carWash.queue.isEmpty === true)
//            carWash.close()
//            if(  getSick(m)) Iam(AtHome)
//            else if (eod)    Iam(AtHome)
//            else             Iam(AtTeaRoom(i)) + carWash.nbCarsWashed - n0
//          }
//          case AtHome => {
//            m print ("...finally")
//            0.0
//          }
//          case _ => {
//            println(state)
//            throw new RuntimeException()
//          }
//        }
//      }
//    }
//
//    class Car(m: Model[Unit], carWash: CarWash, id: String) extends Process[Unit](id)(m) {
//      def start(): Unit @cpsParam[Option[Unit],Option[Unit]] = {
//        m print (id + ": arrives")
//        if (!carWash.request[Unit]()) {
//          m print (id + ": Hoooo, I've been refused")
//        } else {
//          m print (id + ": I'm clean!")
//          carWash.release()
//        }
//      }
//    }
//
//    class CarWashObserver(cw: CarWash) {
//      var lastTime = cw.m.clock()
//      var timeWorking: Long = 0
//      whenever(cw.isOpened === false) {
//        timeWorking += cw.m.clock() - lastTime
//        lastTime = cw.m.clock()
//      }
//    }
//    class CarWasherObserver(w: CarWasher) {
//      var lastEntered: Long = 0
//      var totalTimeSpentAtTeaRoom: Long = 0
//      whenever(w.entering) { s =>
//        s match {
//          case AtTeaRoom => totalTimeSpentAtTeaRoom += w.m.clock() - lastEntered
//          case _         => lastEntered = w.m.clock()
//        }
//        println(s)
//      }
//
//    }
//
//    val carWash = new CarWash(m)
//    val carWasher = new CarWasher(m, carWash)
//
//    var i = 0
//
//    val carGenerator = Generator(new ExponentialD( 1.0 / 20.minute.toLong )) {
//      val c = new Car(m, carWash, "car:" + i)
//      i += 1
//      c.simulate()
//    }
//
//    val obs = new CarWashObserver(carWash)
//    val obs2 = new CarWasherObserver(carWasher)
//    m.simulate(10.hour, false)
//
//    efficiency observe ((8.hour - obs2.totalTimeSpentAtTeaRoom) * 1.0 / 8.hour)
//    //for ( en <- carWasher.results){println(en)}
//  }
//
//}
//
