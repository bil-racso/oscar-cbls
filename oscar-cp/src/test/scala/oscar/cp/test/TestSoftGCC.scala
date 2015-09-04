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
package oscar.cp.test

import oscar.cp._
import oscar.cp.testUtils._
import oscar.cp.constraints.{SoftGCCFWC, SoftGCC}

import scala.util.Random

/**
 * @author Victor Lecomte
 */
class TestSoftGCC extends TestSuite {

  val AC = 0
  val FWC = 1

  def nbSol(domX: Array[Set[Int]], values: Range, lower: Array[Int], upper: Array[Int], domViol: Set[Int],
            mode: Int): (Int, Long, Int, Int) = {
    val cp = CPSolver()

    val nVariables = domX.length
    val X = Array.tabulate(nVariables)(i => CPIntVar(domX(i))(cp))
    val viol = CPIntVar(domViol)(cp)

    try {
      if (mode == AC) {
        cp.add(new SoftGCC(X, values.min, lower, upper, viol))
      } else {
        cp.add(new SoftGCCFWC(X, values.min, lower, upper, viol))
      }
    } catch {
      case e: oscar.cp.core.NoSolutionException => return (0, 0, 0, 0)
    }

    cp.search { binaryStatic(X :+ viol) }

    val stat = cp.start(nSols = 10000000)
    (stat.nSols, stat.time, stat.nNodes, stat.nFails)
  }

  var rand: Random = null
  def randomDom(size: Int) = Array.fill(size)(rand.nextInt(size)).toSet

  // nSols on random domains
  test("Test #1: Random bounds") {
    for (i <- 1 to 100) {
      println(i)
      rand =  new scala.util.Random(i)

      val nVariables = 8

      val domVars = Array.fill(nVariables)(randomDom(size = 6))
      val domViol = randomDom(size = 5)
      /*for (i <- 0 until nVariables) {
        println(s"$i: ${domVars(i) mkString " "}")
      }*/

      val min = domVars.flatten.min
      val max = domVars.flatten.max
      val nValues = (min to max).length

      val lower = (for (v <- min to max) yield rand.nextInt(3)).toArray
      val upper = lower.map(v => v+rand.nextInt(3))

      val (nSols1, time1, nNodes1, nFails1) = nbSol(domVars, min to max, lower, upper, domViol, AC)
      val (nSols2, time2, nNodes2, nFails2) = nbSol(domVars, min to max, lower, upper, domViol, FWC)

      nSols1 should be(nSols2)
    }
  }
}
