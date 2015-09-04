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
import oscar.cp.constraints.{PrefixCCFWC2, PrefixCCFWC}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * @author Victor Lecomte
 */
class TestPrefixCC extends TestSuite {

  val MULTIPLE_GCC_AC = 0
  val MULTIPLE_GCC_FWC = 1
  val PREFIX_CC_FWC = 2
  val PREFIX_CC_FWC2 = 3

  def nbSol(domX: Array[Set[Int]], values: Range, lower: Array[Array[(Int, Int)]], upper: Array[Array[(Int, Int)]],
            mode: Int): (Int, Long, Int, Int) = {
    val cp = CPSolver()

    //var solSet = Set[String]()

    val nVariables = domX.length
    val nValues = values.length
    val X = Array.tabulate(nVariables)(i => CPIntVar(domX(i))(cp))

    try {
      if (mode == MULTIPLE_GCC_AC || mode == MULTIPLE_GCC_FWC) {

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
      } else if (mode == PREFIX_CC_FWC) {
        cp.add(new PrefixCCFWC(X, values.min, lower, upper))
      } else {
        cp.add(new PrefixCCFWC2(X, values.min, lower, upper))
      }

    } catch {
      case e: oscar.cp.core.NoSolutionException => return (0, 0, 0, 0)
    }

    cp.search { binaryStatic(X) }

    val stat = cp.start(nSols = 10000000)
    (stat.nSols, stat.time, stat.nNodes, stat.nFails)
  }

  var rand: Random = null
  def randomDom(size: Int) = Array.fill(size)(rand.nextInt(size)/*-3*/).toSet
  def randomPlaces(nVariables: Int, size: Int) = Array.fill(size)(rand.nextInt(nVariables) + 1).toSet
  def occurrences(solution: Array[Int], v: Int, i: Int): Int = {
    solution.splitAt(i)._1.count(_ == v)
  }
  def ifPossibleAtLeast(max: Int, atLeast: Int): Int = {
    if (atLeast > max) max
    else max - rand.nextInt(max - atLeast + 1)
  }
  def ifPossibleAtMost(min: Int, atMost: Int): Int = {
    if (atMost < min) min
    else min + rand.nextInt(atMost - min + 1)
  }

  // nSols on random domains
  test("PrefixCC Test #1: random bounds") {
    for (i <- 1 to 100) {
      println(i)
      rand =  new scala.util.Random(i)

      val nVariables = 9

      val domVars = Array.fill(nVariables)(randomDom(size = 6))
      /*for (i <- 0 until nVariables) {
        println(s"$i: ${domVars(i) mkString " "}")
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

      val (nSols1, time1, nNodes1, nFails1) = nbSol(domVars, min to max, lower, upper, MULTIPLE_GCC_FWC)
      val (nSols2, time2, nNodes2, nFails2) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_FWC)
      val (nSols3, time3, nNodes3, nFails3) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_FWC2)

      nSols1 should be(nSols2)
      println(time1, time3)
      nSols1 should be(nSols3)
    }
  }

  test("Test #2: compatible bounds") {
    var (total1, total2, total3) = (0L, 0L, 0L)
    for (i <- 1 to 10) {
      rand =  new scala.util.Random(i)

      val nVariables = 12
      val nValuesMax = 6
      val nBounds = 11
      val minSignificance = 4

      val domVars = Array.fill(nVariables)(randomDom(nValuesMax))
      val modelSolution = domVars.map(s => s.toVector(rand.nextInt(s.size)))

      val min = domVars.flatten.min
      val max = domVars.flatten.max
      val nValues = (min to max).length

      val lowerBuffer = Array.fill(nValues)(ArrayBuffer[(Int,Int)]())
      val upperBuffer = Array.fill(nValues)(ArrayBuffer[(Int,Int)]())

      randomPlaces(nVariables, size = nBounds / 2).foreach(i => {
        val vi = rand.nextInt(nValues)
        //lowerBuffer(vi) += ((i, occurrences(modelSolution, vi + min, i)))
        lowerBuffer(vi) += ((i, ifPossibleAtLeast(occurrences(modelSolution, vi + min, i), minSignificance)))
      })
      randomPlaces(nVariables, size = nBounds / 2).foreach(i => {
        val vi = rand.nextInt(nValues)
        //upperBuffer(vi) += ((i, occurrences(modelSolution, vi + min, i)))
        upperBuffer(vi) += ((i, ifPossibleAtMost(occurrences(modelSolution, vi + min, i), i - minSignificance)))
      })

      val lower = lowerBuffer.map(_.toArray)
      val upper = upperBuffer.map(_.toArray)

      val (nSols1, time1, nNodes1, nFails1) = nbSol(domVars, min to max, lower, upper, MULTIPLE_GCC_FWC)
      val (nSols2, time2, nNodes2, nFails2) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_FWC)
      val (nSols3, time3, nNodes3, nFails3) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_FWC2)
      //val (nSols2, time2, nNodes2, nFails2) = (nSols3, time3, nNodes3, nFails3)
      //val (nSols1, time1, nNodes1, nFails1) = (nSols2, time2, nNodes2, nFails2)


      println(s"$i: time ($time1, $time2, $time3), $nSols1 sols")
      if (nNodes1 != nNodes2 || nNodes2 != nNodes3 || nFails1 != nFails2 || nFails2 != nFails3) {
        println(s"nodes: $nNodes1, $nNodes2, $nNodes3")
        println(s"fails: $nFails1, $nFails2, $nFails3")
      }
      total1 += time1
      total2 += time2
      total3 += time3

      nSols1 should be(nSols2)
    }
    println(s"total time: GCCFWC: $total1, PrefixCCFWC $total2, PrefixCCFWC2 $total3")
  }
}
