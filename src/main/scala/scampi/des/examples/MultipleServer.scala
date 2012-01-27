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
import scala.collection.immutable.Vector
import JSci.maths.statistics._
import scala.util.continuations._

/**
 * @author Jean-Christophe Van den Schrieck
 */

object MultipleServer {

  abstract class Patient(arrival: Double) {
    val name: String
    def processingTime(): Int

  }

  class PatientA(arrival: Double) extends Patient(arrival) {
    val name = "A " + arrival
    println("patient A created")
    def processingTime() = 4

  }

  class PatientB(arrival: Double) extends Patient(arrival) {
    val name = "B " + arrival
    println("patient B created")
    def processingTime() = 9
  }

  class Generator(val m: Model, val freq: Int, val queue: java.util.Vector[Patient], name: String, val create: () => Patient) {

    def generateNextPatient() {
      reset{
      m.wait(freq)
      val p = create()
      println("patient " + p.name + " added from generator" + name)
      queue.add(p)
      generateNextPatient()
      }
    }

    def run() {
//      reset {
        generateNextPatient()
//      }
    }
  }

  class Server(val m: Model, val queue: java.util.Vector[Patient], name: String, nextBreak: Int) {

    var nextBreakStart = nextBreak

    def serveNextPatient(): Unit @suspendable = {

      if (m.clock() >= nextBreakStart) {
        takeBreak()
      } else {
        if (queue.isEmpty()) {
          m.wait(1)
          println("no patient " + name + "drank a coffee")
          serveNextPatient()

        } else {
          val p: Patient = queue.elementAt(0)
          queue.remove(0)
          m.wait(p.processingTime())
          println("patient" + p.name + " processed at " + m.clock() + " by " + name)
          serveNextPatient()

        }
      }
    }

    def takeBreak(): Unit @suspendable = {
      println(name + "starts his break")
      m.wait(50.0 - (m.clock() - nextBreakStart))
      println(name + "is back to work")
      nextBreakStart = nextBreakStart + 100
      serveNextPatient()

    }

    def run() {
      reset {
        serveNextPatient()
      }
    }
  }

  def main(args: Array[String]) {

    val lgnorm = new LognormalDistribution()

    val mod = new Model()
    val queue = new java.util.Vector[Patient]()
    val gen1 = new Generator(mod, 10, queue, "gen1", () => new PatientA(mod.clock()))
    gen1.run()
    val gen2 = new Generator(mod, 7, queue, "gen2", () => new PatientB(mod.clock()))
    gen2.run()

    val ariane1 = new Server(mod, queue, "Ariane1", 0)
    ariane1.run()
    val ariane2 = new Server(mod, queue, "Ariane2", 50)
    ariane2.run()

    mod.simulate(200, true);
    println(55)

  }
}