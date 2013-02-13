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
import collection.immutable.SortedSet

class TestDudeney extends FunSuite with ShouldMatchers  {


  test("Dudeney") {
    
    val sol = Set(1,512,4913,5832,17576,19683)
      
    val n = 5

    val cp = new CPSolver()
    var nbSol = 0

    val x = (0 until n).map(v => CPVarInt(cp, 0 to 9))
    val nb = CPVarInt(cp, 1 to math.pow(10, n).toInt - 1)
    val s = CPVarInt(cp, 1 to 9 * n)

    cp.solve subjectTo {
      cp.add(nb == (s mul s mul s))
      cp.add(sum(0 until n)(i => x(i) * (math.pow(10, (n - i - 1)).toInt)) == nb)
      cp.add(sum(x) == s)
    } exploration {
      cp.binaryFirstFail(x)
      sol.contains(nb.value) should be(true)
      nbSol += 1
    } run()
    nbSol should be (sol.size)

    
  }  
  

  


}
