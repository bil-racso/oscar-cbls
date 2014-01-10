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

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._


class TestGCCFWC extends FunSuite with ShouldMatchers {


  test("Test 3: Variation of 2 that should have some solutions") {
    val cp = new CPSolver()
    var x1 = CPVarInt(0, 2)(cp)
    var x2 = CPVarInt(1, 3)(cp)
    var x3 = CPVarInt(0, 3)(cp)
    val x = Array(x1, x2, x3)

    val minVal = 0
    val Low = Array(0, 0, 0)
    val Up = Array(2, 0, 0)

    var nbSol = 0;
    cp.solve subjectTo {
      cp.add(new GCCFWC(x, minVal, Low, Up))
    } search {
      binaryFirstFail(x)
    }

    cp.start().nSols should be > 0
  }
  
  test("Test 4: verifying a solution") {
    val cp = new CPSolver()
    var x1 = CPVarInt(0, 2)(cp)
    var x2 = CPVarInt(1, 3)(cp)
    var x3 = CPVarInt(0, 3)(cp)
    val x = Array(x1, x2, x3)

    // T4
    val minVal = 0
    val Low = Array(0, 1, 0, 2)
    val Up = Array(3, 2, 3, 2)

    cp.add(new GCCFWC(x, minVal, Low, Up))

    val sol = Array[Int](1, 3, 3)
    for (i <- 0 until x.length) {
      x(i).value should be(sol(i))
    }
  }

  test("Test 5: counting solutions") {
    val cp = new CPSolver()
    var x1 = CPVarInt(0, 2)(cp)
    var x2 = CPVarInt(1, 3)(cp)
    var x3 = CPVarInt(0, 3)(cp)
    val x = Array(x1, x2, x3)

    val minVal = 0
    val Low = Array(0, 0, 0, 2)
    val Up = Array(0, 2, 3, 2)

    var nbSol = 0;
    cp.solve subjectTo {
      cp.add(new GCCFWC(x, minVal, Low, Up))
    } search {
      binaryFirstFail(x)
    }

    cp.start().nSols should be(2)
  }

  test("Test 6: counting solutions") {
    val cp = new CPSolver()
    var x1 = CPVarInt(0, 2)(cp)
    var x2 = CPVarInt(1, 3)(cp)
    var x3 = CPVarInt(0, 3)(cp)
    val x = Array(x1, x2, x3)

    val minVal = 0
    val Low = Array(0, 0, 0, 0)
    val Up = Array(1, 1, 1, 1)

    var nbSol = 0;
    cp.solve subjectTo {
      cp.add(new GCCFWC(x, minVal, Low, Up))
    } search {
      binaryFirstFail(x)
    }

    cp.start().nSols should be(14)

  }
}

