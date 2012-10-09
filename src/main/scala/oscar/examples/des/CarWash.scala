/*******************************************************************************
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
 ******************************************************************************/

package oscar.examples.des

import oscar.des.engine._
import oscar.des.engine
import oscar.invariants._
import scala.util.continuations._
import JSci.maths.statistics._

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
object CarWash {

  def main(args: Array[String]) {

    implicit val m = new Model()

    // one day = 8 hours
    val endOfDay = m.clock === 480

    class CarWash(m: Model) {
      val queue = new SimQueue()
      once(endOfDay) { _ => queue.close() }

      val isWorking = new Var[Boolean](false)

      def request(): Boolean @suspendable = {
        if ( !queue.enter ) cpsfalse
        else {
           waitFor(isWorking === true)
           waitFor(m.clock === m.clock() + 10)
           cpstrue
        }
      }
      
      def release() { queue.leave }
      
      def open() {
        isWorking := true
      }
      
      def close() { isWorking := false }

    } // end of CarWash

    class CarWasher(m: Model, carWash: CarWash) extends Process("Washer")(m) {

      var eod = false
      once(endOfDay){ _=>eod = true }
      def atTearoom(): Unit @suspendable = {
        m print("washer at tea room")
        println(carWash.queue.isEmpty())
        val opt = waitFor( carWash.queue.isEmpty === false | endOfDay )
        if (opt == 1) atEndOfDay()
        else {
          atCarWash()
        }
      }

      def atCarWash(): Unit @suspendable = {
        m print("washer at car wash")
        carWash.open()
        waitFor( carWash.queue.isEmpty === true )
        carWash.close()
        if ( eod ) atEndOfDay()
        else atTearoom()
      }
      def atEndOfDay(): Unit @suspendable =  {
        m print("...finally")
      }

      def start() = {
        carWash.queue.open()
        //carWash.open()
        atTearoom()
      }

    }   
    
    class Car(m: Model, carWash: CarWash, id: String) extends Process(id)(m) {
      def start(): Unit @suspendable = {
        m print(id + ": arrives")
        if (!carWash.request()) {
          m print(id + ": Hoooo, I've been refused")
        } else {
          m print(id + ": I'm clean!")
          carWash.release()
        }
      }
    }

    val carWash = new CarWash(m)

    val carWasher = new CarWasher(m,carWash)

    var i = 0

    
    val carGenerator = Generator(new ExponentialDistribution(1.0 / 11)) {
      val c = new Car(m, carWash, "car:" + i)
      i += 1
      c.simulate()
    }

    m.simulate(650, false)
  }
}
