package scampi.des.examples

import scampi.des.engine._
import scampi.invariants._
import scala.util.continuations._
import JSci.maths.statistics._

object CarWash {

  def main(args: Array[String]) {

    val m = new Model()

    // one day = 8 hours
    val endOfDay = m.clock === 480

    class CarWash(m: Model) {
      val queue = new SimQueue
      once(endOfDay) { _ => queue.close() }

      val isWorking = new Var[Boolean](false)

      def request(): Boolean @suspendable = {
        if ( !queue.enter ) cpsfalse
        else{waitFor(isWorking === true)
        waitFor(m.clock === m.clock() + 10)
        cpstrue
        }

      }
      def release(){queue.leave}
      def open() {
        isWorking := true
      }
      def close() { isWorking := false }

    }

    class CarWasher(m: Model, carWash: CarWash) extends Process(m, "Washer") {

      var eod = false
      once(endOfDay){_=>eod = true}
      def atTearoom(): Unit @suspendable = {
        m print("washer at tea room")
        println(carWash.queue.isEmpty())
        val opt = waitFor( carWash.queue.isEmpty === false | endOfDay)
        if (opt == 1) atEndOfDay()
        else {
          atCarWash()
        }
      }

      def atCarWash(): Unit @suspendable = {
        m print("washer at car wash")
        carWash.open()
        waitFor(carWash.queue.isEmpty === true)
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
    class Car(m: Model, carWash: CarWash, id: String) extends Process(m, id) {
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

    val carWasher = new CarWasher(m, carWash)

    var i = 0
    val carGenerator = new Generator(m, new ExponentialDistribution(1.0 / 11), {
      val c = new Car(m, carWash, "car:" + i)
      i += 1
      c.simulate()
    })

    m.simulate(650, false)
  }
}