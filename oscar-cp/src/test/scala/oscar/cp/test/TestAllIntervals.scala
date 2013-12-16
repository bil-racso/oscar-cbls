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

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.modeling._
import oscar.algo.search._
import oscar.cp.core._
import oscar.cp.scheduling._
import oscar.cp.constraints._

class TestAllIntervals extends FunSuite with ShouldMatchers {

  test("AllIntervals") {

    val cp = CPSolver()

    //
    // data
    //
    val n = 6

    println("n: " + n)

    //
    // variables
    //

    val x = Array.fill(n)(CPVarInt(cp, 0 to n - 1))
    val diffs = Array.fill(n - 1)(CPVarInt(cp, 1 to n - 1))

    //
    // constraints
    //
    var numSols = 0
    cp.solve subjectTo {

      cp.add(allDifferent(diffs), Strong)
      cp.add(allDifferent(x), Strong)

      for (k <- 0 until n - 1) {
        cp.add(diffs(k) == (x(k + 1) - (x(k))).abs())
      }

      // symmetry breaking
      cp.add(x(0) < x(n - 1))
      cp.add(diffs(0) < diffs(1))

    } exploration {
      println("exploration")
      cp.binary(x)

      print("x:" + x.mkString(""))
      print("  diffs:" + diffs.mkString(""))
      println()

      numSols += 1

    } run ()
    numSols should be(8)
    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

 
}
