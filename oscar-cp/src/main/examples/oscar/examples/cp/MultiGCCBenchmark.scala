/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.examples.cp

import oscar.cp.constraints.{GCCFWC, GCCBC, SoftGCC}
import oscar.cp.core.{Constraint, CPSolver}
import oscar.cp.core.variables.CPIntVar

/**
 * Allows easy creation of GCC comparative benchmarks.
 * All you have to do is implement the run() method, as if it were a typical
 * subclass of CPModel and App.
 * @author Victor Lecomte
 */
trait MultiGCCBenchmark extends App {

  object Mode extends Enumeration {
    type Mode = Value
    val Soft, BC, FWC = Value
  }
  import Mode._

  def init() {}
  def run()

  init()

  var mode = Soft
  implicit var cp = CPSolver()
  println("SoftGCC:")
  run()
  println()

  mode = BC
  cp = CPSolver()
  println("GCCBC:")
  //run()
  println()

  mode = FWC
  cp = CPSolver()
  println("GCCFWC:")
  run()
  println()

  def gcc(x: Array[CPIntVar], values: Range, lower: Int, upper: Int): Constraint = {
    gcc(x, values, Array.fill(values.size)(lower), Array.fill(values.size)(upper))
  }
  def gcc(x: Array[CPIntVar], values: Range, lower: Array[Int], upper: Array[Int]): Constraint = {
    if (mode == Soft) {
      new SoftGCC(x, values.min, lower, upper, CPIntVar(0, 0))
    } else if (mode == BC) {
      var bounds: Map[Int, (Int, Int)] = Map()
      for (i <- values.indices) {
        bounds += (values(i) -> (lower(i), upper(i)))
        //println((values(i), lower(i), upper(i)))
      }
      //println()
      new GCCBC(x, bounds)
    } else {
      new GCCFWC(x, values.min, lower, upper)
    }
  }
}