/**
 * *****************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *
 * Contributors:
 *      www.n-side.com
 * ****************************************************************************
 */
package scampi.des.examples

import scampi.des.engine._
import scala.util.continuations._
import scala.react._

/**
 * Two machines can be broken, they are two repair person to fix it so it can be done in parallel
 * @author pschaus
 */

class Machine1(m: Model, name: String) extends Process(m, name) {

  val atLiving = new EventSource[Int]
  val atEndLiving = new EventSource[Unit]
  val atDeath = new EventSource[Unit]

  val live = new State[Int]() {
    override def code(i:Int): Unit@suspendable = {
      println("waiting")
      m.wait(10)
    }
  }

  def broken(): Unit @suspendable = {
    m.wait(24)
  }

  def repair(): Unit @suspendable = {
    m.wait(2)
  }

  def run() {
    reset {
      val r = 1 to 5
      val iter = r.iterator()
      while (iter.hasNext()) {
        live.run(iter.next())
        broken
      }
    }
  }

}

object Machine1 extends Observing {
  def main(args: Array[String]) {
    val mod = new Model()
    val m1 = new Machine1(mod, "machine1")

    val f1 = mod.frequency(m1.live)

        Reactor.once { self =>
          self.loopUntil(m1.atDeath) {
            println("m1" + " is working for the " + self.next(m1.live.atEntry) + "rd time");
            self delay
          }
          println("Hooo, m1 is too old")
        }

    m1.run()
    mod.simulate(200, true);
        println("m1 lived " + f1() * 100 + "% of the time")
        println()
  }
}