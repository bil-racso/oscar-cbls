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
import oscar.cp.search._
import oscar.cp.modeling._
import oscar.cp.scheduling.Activity

import org.scalacheck._

class TestUnary extends FunSuite with ShouldMatchers  {


  test("Unary 1") {
    val horizon = 5
    val cp = CPSolver()

    val act1 = new Activity(CPVarInt(cp,0 to 5), 3) //Should be reduce to 3
    val act2 = new Activity(CPVarInt(cp,0 to 5), 2)
    val act3 = new Activity(CPVarInt(cp,0 to 5), 2)
    val act4 = new Activity(CPVarInt(cp,0 to 5), 1)
    val acts = Array(act1, act2, act3, act4)
    
    var starts = acts.map(_.start)

    cp.add(new UnaryResource(Array(act1,act2), "res1"));
    cp.add(new UnaryResource(Array(act3,act4), "res2"));

    val expectedSol = Set((0, 3, 0, 2),
                        (0, 3, 0, 3),
                        (0, 3, 0, 4),
                        (0, 3, 1, 0),
                        (0, 3, 1, 3),
                        (0, 3, 1, 4),
                        (0, 3, 2, 0),
                        (0, 3, 2, 1),
                        (0, 3, 2, 4),
                        (0, 3, 3, 0),
                        (0, 3, 3, 1),
                        (0, 3, 3, 2),
                        (2, 0, 0, 2),
                        (2, 0, 0, 3),
                        (2, 0, 0, 4),
                        (2, 0, 1, 0),
                        (2, 0, 1, 3),
                        (2, 0, 1, 4),
                        (2, 0, 2, 0),
                        (2, 0, 2, 1),
                        (2, 0, 2, 4),
                        (2, 0, 3, 0),
                        (2, 0, 3, 1),
                        (2, 0, 3, 2))

    var nbSol = 0
    cp.solveAll subjectTo {
      acts.foreach(a => cp.add(a.end <= 5))  
    } exploration {
      cp.binary(starts)
      val sol = (act1.est, act2.est, act3.est, act4.est)
      expectedSol.contains(sol) should be(true)
      nbSol += 1
    }
    println("nbSol="+nbSol)
    nbSol should be(24)
 }
}
