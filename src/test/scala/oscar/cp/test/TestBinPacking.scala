/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._


class TestBinPacking extends FunSuite with ShouldMatchers  {


  test("BP 1") {
    val binCapacities = Array(2 to 3, 45 to 55, 0 to 0)
    val itemsSizes = Array(3, 10, 12, 18, 10)
    val binForItems = Array(
      Array(0, 1),
      Array(1, 2),
      Array(1, 2),
      Array(1, 0),
      Array(1, 0))

    val items = 0 until itemsSizes.length
    val bins = 0 until binCapacities.length

    val cp = new CPSolver()
    val x = (for (i <- items) yield CPVarInt(cp, binForItems(i))).toArray
    val l = bins.map(i => CPVarInt(cp, binCapacities(i))).toArray
    var nbSol = 0
    cp.solve 
    cp.exploration {
      cp.binary(x)
      nbSol += 1
    }
    nbSol = 0
    cp.runSubjectTo() {
      cp.add(new BinPacking(x, itemsSizes, l), Weak)
    }
    nbSol should be(1)
    nbSol = 0
    cp.runSubjectTo() {
      cp.add(new BinPacking(x, itemsSizes, l), Strong)
    }
    nbSol should be(1)
  }
  
 
  
 
}
  
  
