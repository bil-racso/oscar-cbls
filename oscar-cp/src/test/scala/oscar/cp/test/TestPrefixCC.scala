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
import oscar.cp.constraints._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * @author Victor Lecomte
 */
class TestPrefixCC extends TestSuite {

  val MULTIPLE_GCC_AC = 0
  val MULTIPLE_GCC_FWC = 1
  val PREFIX_CC_SEGMENTS = 3
  val PREFIX_CC_FENWICK = 4
  val PREFIX_CC_FWC3 = 5

  def makeGccs(X: Array[CPIntVar], values: Range, lower: Array[Array[(Int, Int)]], upper: Array[Array[(Int, Int)]],
               mode: Int): Array[Constraint] = {
    val nVariables = X.length
    val nValues = values.length

    if (mode == MULTIPLE_GCC_AC || mode == MULTIPLE_GCC_FWC) {
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

      for (i <- cutoffs.toArray)
        yield new GCC(X.splitAt(i)._1, values.min, allLower(i), allUpper(i))

    } else if (mode == PREFIX_CC_SEGMENTS) {
      Array(new PrefixCCSegments(X, values.min, lower, upper))
    } else if (mode == PREFIX_CC_FENWICK) {
      Array(new PrefixCCFenwick(X, values.min, lower, upper))
    } else {
      Array(new PrefixCCFWC3(X, values.min, lower, upper))
    }
  }

  def nbSol(domX: Array[Set[Int]], values: Range, lower: Array[Array[(Int, Int)]], upper: Array[Array[(Int, Int)]],
            mode: Int): (Int, Long, Int, Int) = {
    val cp = CPSolver()

    val X = domX.map(dom => CPIntVar(dom)(cp))

    val strength = {
      if (mode == MULTIPLE_GCC_AC) CPPropagStrength.Strong
      else CPPropagStrength.Weak
    }

    for (con <- makeGccs(X, values, lower, upper, mode))
      cp.post(con, strength)

    cp.search { binaryStatic(X) }

    val stat = cp.start(nSols = 1000000)
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

  def lowerAtMostBy(max: Int, error: Double): Int = {
    max - rand.nextInt((max * error).toInt + 1)
  }
  def higherAtMostBy(min: Int, error: Double): Int = {
    min + rand.nextInt((min * error).toInt + 1)
  }

  // nSols on random domains
  test("Random bounds") {
    for (i <- 1 to 100) {
      //println(s"test #$i")
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
      val (nSols2, time2, nNodes2, nFails2) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_SEGMENTS)
      val (nSols3, time3, nNodes3, nFails3) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_FENWICK)
      val (nSols4, time4, nNodes4, nFails4) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_FWC3)
      //val (nSols3, time3, nNodes3, nFails3) = (nSols4, time4, nNodes4, nFails4)
      //val (nSols2, time2, nNodes2, nFails2) = (nSols3, time3, nNodes3, nFails3)
      //val (nSols1, time1, nNodes1, nFails1) = (nSols2, time2, nNodes2, nFails2)

      nSols1 should be(nSols2)
      nSols1 should be(nSols3)
      nSols1 should be(nSols4)
    }
  }

  test("Compatible bounds") {
    var (total1, total2, total3, total4) = (0L, 0L, 0L, 0L)
    for (i <- 1 to 100) {
      //println(s"test #$i")
      rand =  new scala.util.Random(i)

      val nVariables = 12
      val nValuesMax = 4
      val nBounds = 10
      val minSignificance = 4
      //val stupidFactor = 7

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
        //lowerBuffer(vi) += ((i, i / stupidFactor))
      })
      randomPlaces(nVariables, size = nBounds / 2).foreach(i => {
        val vi = rand.nextInt(nValues)
        //upperBuffer(vi) += ((i, occurrences(modelSolution, vi + min, i)))
        upperBuffer(vi) += ((i, ifPossibleAtMost(occurrences(modelSolution, vi + min, i), i - minSignificance)))
        //upperBuffer(vi) += ((i, i - i / stupidFactor))
      })

      val lower = lowerBuffer.map(_.toArray)
      val upper = upperBuffer.map(_.toArray)

      val (nSols1, time1, nNodes1, nFails1) = nbSol(domVars, min to max, lower, upper, MULTIPLE_GCC_FWC)
      val (nSols2, time2, nNodes2, nFails2) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_SEGMENTS)
      val (nSols3, time3, nNodes3, nFails3) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_FENWICK)
      val (nSols4, time4, nNodes4, nFails4) = nbSol(domVars, min to max, lower, upper, PREFIX_CC_FWC3)
      //val (nSols3, time3, nNodes3, nFails3) = (nSols4, time4, nNodes4, nFails4)
      //val (nSols2, time2, nNodes2, nFails2) = (nSols3, time3, nNodes3, nFails3)
      //val (nSols1, time1, nNodes1, nFails1) = (nSols2, time2, nNodes2, nFails2)


      /*println(s"$i: time ($time1, $time2, $time3, $time4), $nSols1 sols")
      if (nNodes1 != nNodes2 || nNodes2 != nNodes3 || nFails1 != nFails2 || nFails2 != nFails3) {
        println(s"nodes: $nNodes1, $nNodes2, $nNodes3, $nNodes4")
        println(s"fails: $nFails1, $nFails2, $nFails3, $nFails4")
      }*/
      total1 += time1
      total2 += time2
      total3 += time3
      total4 += time4

      nSols1 should be(nSols2)
      nSols1 should be(nSols3)
      nSols1 should be(nSols4)
    }
    //println(s"total time: GCCFWC: $total1, PrefixCCFWC $total2, PrefixCCFWC2 $total3, PrefixCCFWC3 $total4")
  }

  /* Code used to compare the performance of the different approaches and choose which one to keep.
  See performance comparison at https://github.com/vlecomte/prefixcc-tech-report/blob/master/perf-comparison.pdf
  test("Performance profile") {
    val modes = Array(MULTIPLE_GCC_FWC, PREFIX_CC_SEGMENTS, PREFIX_CC_FENWICK, PREFIX_CC_FWC3)
    val modeNames = Array("Multiple GCCs", "PrefixCCFWC", "PrefixCCFWC2", "PrefixCCFWC3")
    val resultsTime = Array.fill(modes.length)(new ArrayBuffer[Long]())
    val resultsBacktrack = Array.fill(modes.length)(new ArrayBuffer[Int]())

    for (batch <- 1 to 10) {
      println()
      println(s"batch #$batch")
      println()
      val hard = false

      for ((nVariables, i) <- Seq(50,100,200).zipWithIndex) {
        rand = new scala.util.Random(batch * 1000 + i)
        println(s"test #${i + 1}: $nVariables variables")

        val nValuesMax = 3
        val nBounds = 2 * nVariables
        val stupidFactor = 7
        val boundFraction = 0.25

        val domX = Array.fill(nVariables)(randomDom(nValuesMax))
        val modelSolution = domX.map(s => s.toVector(rand.nextInt(s.size)))

        val min = domX.flatten.min
        val max = domX.flatten.max
        val values = min to max
        val nValues = values.length

        val lowerBuffer = Array.fill(nValues)(ArrayBuffer[(Int, Int)]())
        val upperBuffer = Array.fill(nValues)(ArrayBuffer[(Int, Int)]())

        val lowerPlaces = randomPlaces(nVariables, size = nBounds / 2)
        val upperPlaces = randomPlaces(nVariables, size = nBounds / 2)
        lowerPlaces.foreach(i => {
          val vi = rand.nextInt(nValues)
          //lowerBuffer(vi) += ((i, occurrences(modelSolution, vi + min, i)))
          //lowerBuffer(vi) += ((i, lowerBy(occurrences(modelSolution, vi + min, i), boundFraction)))
          if (hard) lowerBuffer(vi) += ((i, lowerAtMostBy(occurrences(modelSolution, vi + min, i), boundFraction)))
          else lowerBuffer(vi) += ((i, i / stupidFactor))
        })
        upperPlaces.foreach(i => {
          val vi = rand.nextInt(nValues)
          //upperBuffer(vi) += ((i, occurrences(modelSolution, vi + min, i)))
          //upperBuffer(vi) += ((i, higherBy(occurrences(modelSolution, vi + min, i), boundFraction)))
          if (hard) upperBuffer(vi) += ((i, higherAtMostBy(occurrences(modelSolution, vi + min, i), boundFraction)))
          else upperBuffer(vi) += ((i, i - i / stupidFactor))
        })

        val lower = lowerBuffer.map(_.toArray)
        val upper = upperBuffer.map(_.toArray)

        val decisionOrder = rand.shuffle((0 until nVariables).toList).toArray

        val solLimit = 50000000 / nVariables
        val player = (mode: Int) => nbSol2(domX, values, lower, upper, solLimit, decisionOrder, mode)

        val runningOrder = rand.shuffle(modes.indices.toList).toArray
        for (i <- runningOrder) {
          println(modeNames(i))
          val (time, backtracks) = player(modes(i))
          resultsTime(i) += time
          resultsBacktrack(i) += backtracks
        }
        println()
      }
    }
    for (i <- modes.indices) {
      println(s"results for ${modeNames(i)} (time then backtracks)")
      println(resultsTime(i).toArray.mkString(", "))
      println(resultsBacktrack(i).toArray.mkString(", "))
    }
  }

  def nbSol2(domX: Array[Set[Int]], values: Range, lower: Array[Array[(Int, Int)]], upper: Array[Array[(Int, Int)]],
            solLimit: Int, decisionOrder: Array[Int], mode: Int): (Long, Int) = {
    val cp = CPSolver()
    val X = domX.map(dom => CPIntVar(dom)(cp))

    val strength = {
      if (mode == MULTIPLE_GCC_AC) CPPropagStrength.Strong
      else CPPropagStrength.Weak
    }

    for (con <- makeGccs(X, values, lower, upper, mode))
      cp.post(con, strength)

    cp.search { binaryStatic(decisionOrder.map(X(_))) }

    val stat = cp.start(nSols = solLimit, timeLimit = 300)
    println("sols=" + stat.nSols, "time=" + stat.time, "nodes=" + stat.nNodes, "fails=" + stat.nFails)
    (stat.time, stat.nFails)
  }*/
}
