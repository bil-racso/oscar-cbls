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
import oscar.cp.core.CPPropagStrength
import oscar.cp.testUtils._
import oscar.cp.constraints.PrefixCCFWC

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * @author Victor Lecomte
 */
class TestPrefixCC extends TestSuite {

  val MULTIPLE_GCC_AC = 0
  val MULTIPLE_GCC_FWC = 1
  val PREFIX_CC_FWC = 2

  def nbSol(domX: Array[Set[Int]], values: Range, lower: Array[Array[(Int, Int)]], upper: Array[Array[(Int, Int)]],
            mode: Int): (Int, Long, Int) = {
    val cp = CPSolver()

    //var solSet = Set[String]()

    val nVariables = domX.length
    val nValues = values.length
    val X = Array.tabulate(nVariables)(i => CPIntVar(domX(i))(cp))

    try {
      if (mode < PREFIX_CC_FWC) {

        val strength = {
          if (mode == MULTIPLE_GCC_AC) CPPropagStrength.Strong
          else CPPropagStrength.Weak
        }
        var cutoffs = Set[Int]()
        val allLower = Array.tabulate(nVariables + 1, nValues)((i, vi) => 0)
        val allUpper = Array.tabulate(nVariables + 1, nValues)((i, vi) => i)

        for (vi <- values.indices) {
          for ((i, bound) <- lower(vi)) {
            allLower(i)(vi) = bound
            cutoffs += i
          }
          for ((i, bound) <- upper(vi)) {
            allUpper(i)(vi) = bound
            cutoffs += i
          }
        }

        for (i <- cutoffs) {
          cp.add(gcc(X.splitAt(i)._1, values, allLower(i), allUpper(i)), strength)
        }
      } else {
        cp.add(new PrefixCCFWC(X, values.min, lower, upper))
      }

    } catch {
      case e: oscar.cp.core.NoSolutionException => return (0, 0, 0)
    }

    cp.search { binaryStatic(X) }
    //cp.onSolution { solSet += X.mkString(" ") }

    val stat = cp.start()
    (stat.nSols, stat.time, stat.nNodes)
  }

  var rand: Random = null
  def randomDom(size: Int) = Array.fill(size)(rand.nextInt(size)/*-3*/).toSet
  def randomPlaces(nVariables: Int, size: Int) = Array.fill(size)(rand.nextInt(nVariables) + 1).toSet

  // nSols on random domains
  test("PrefixCC Test #1") {
    var (total1, total2) = (0L, 0L)
    for (i <- 0 to 100) {
      rand =  new scala.util.Random(i)

      val nVariables = 8

      val domVars = Array.fill(nVariables)(randomDom(size = nVariables))
      /*println("domains:")
      for (dom <- domVars) {
        println(dom)
      }*/

      val min = domVars.flatten.min
      val max = domVars.flatten.max
      val nValues = (min to max).length

      val lowerBuffer = Array.fill(nValues)(ArrayBuffer[(Int,Int)]())
      val upperBuffer = Array.fill(nValues)(ArrayBuffer[(Int,Int)]())

      randomPlaces(nVariables, size = nVariables / 3).foreach(i => {
        val vi = rand.nextInt(nValues)
        lowerBuffer(vi) += ((i, 1 + rand.nextInt(1 + i / 2)))
      })
      randomPlaces(nVariables, size = nVariables / 3).foreach(i => {
        val vi = rand.nextInt(nValues)
        upperBuffer(vi) += ((i, i - 1 - rand.nextInt(1 + i / 2)))
      })

      val lower = lowerBuffer.map(_.toArray)
      val upper = upperBuffer.map(_.toArray)

      // GCCAC is less accepting of conflicting values of lower and upper bounds
      //val (nSols0, nFails0) = nbSol(domVars, min to max, lower, upper, MULTIPLE_GCC_AC)
      val (nSols1, time1, nNodes1) = nbSol(domVars, min to max, lower, upper, MULTIPLE_GCC_FWC)
      val (nSols2, time2, nNodes2) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_FWC)


      //println(s"$i: sols ($nSols1, $nSols2) time ($time1, $time2) nodes ($nNodes1, $nNodes2)")
      total1 += time1
      total2 += time2
      /*if (nSols1 != nSols2) {
        println("refused sols:")
        for (sol <- solSet1 diff solSet2) {
          println(sol)
        }
      }*/
      nSols1 should be(nSols2)
    }
    println(s"total time: GCCFWC: $total1, PrefixCCFWC $total2")
  }
}
