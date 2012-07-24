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

import org.scalacheck._

class TestSortedness extends FunSuite with ShouldMatchers with CPModel {


  test("Sortedness1") {
    val cp = CPSolver()
    val x_ = Array(5,2,4,1,3)
    val s_ = Array(1,2,3,4,5)
    val p_ = Array(3,1,4,2,0)
    val x = Array.tabulate(x_.size)(i => CPVarInt(cp,x_(i)))
    val s = Array.tabulate(x_.size)(i => CPVarInt(cp,s_(i)))
    val p = Array.tabulate(x_.size)(i => CPVarInt(cp,0 until x.size))
    cp.add(sortedness(x,s,p))
    println(p.mkString(","))
    for (i <- 0 until x.size) {
      p(i).getValue() should be(p_(i))
    }
    cp.getStatus() should not be(CPOutcome.Failure)
  }
  
  test("Sortedness2") {
    val cp = CPSolver()
    val x_ = Array(5,2,4,1,3)
    val s_ = Array(1,2,3,4,5)
    val p_ = Array(3,1,4,2,0)
    val x = Array.tabulate(x_.size)(i => CPVarInt(cp,x_(i)))
    val s = Array.tabulate(x_.size)(i => CPVarInt(cp,0 to 100))
    val p = Array.tabulate(x_.size)(i => CPVarInt(cp,p_(i)))
    cp.add(sortedness(x,s,p))
    println(p.mkString(","))
    for (i <- 0 until x.size) {
      s(i).getValue() should be(s_(i))
    }
    cp.getStatus() should not be(CPOutcome.Failure)
  }
  
  
  

}
  
  
