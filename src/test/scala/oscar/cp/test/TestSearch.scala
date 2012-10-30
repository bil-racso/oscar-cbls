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
import oscar.search.IDSSearchController
import oscar.cp.modeling._

import org.scalacheck._

class TestSearch extends FunSuite with ShouldMatchers  {

  test("ids search, bug #36") {
    val cp = new CPSolver()
    val x = Array(CPVarInt(cp, 0))
    var nbSol = 0
    cp.sc = new IDSSearchController(cp, 4)
    cp.minimize(x(0)) exploration {
      cp.binaryFirstFail(x)
      nbSol += 1
    }
    nbSol should be(1)
  }  
  
    
  
 


}
