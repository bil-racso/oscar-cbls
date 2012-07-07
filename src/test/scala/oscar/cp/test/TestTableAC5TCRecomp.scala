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

class TestTableAC5TCRecomp extends FunSuite with ShouldMatchers with CPModel {


  test("TableAC5 Test 1") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarInt(cp, 1 to 3))
    
    val tuples = Array((1,1,1),
                       (1,2,3))
    
    cp.post(new TableAC5TCRecomp(x(0),x(1),x(2),tuples))
    	
    x(0).isBound() should be(true)
    x(0).getValue() should be(1)
    x(2).hasValue(2) should be (false)
    
    println(x.mkString(","))
    
    cp.post(x(2) != 3)
    println(x.mkString(","))
    
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(1).getValue() should be(1)
    x(2).getValue() should be(1)

  }
  
  test("TableAC5 Test 2") {
    val cp = CPSolver()
    
    var x = CPVarInt(cp, 0 to 4)
    var y = CPVarInt(cp, 0 to 4)
    var z = CPVarInt(cp, 0 to 24)
    
    
    val tuples = (for (i <- 0 until 5; j <- i+1 until 5) yield (i,j,i*4+j-1)).toArray
    cp.post(new TableAC5TCRecomp(x,y,z,tuples))
    cp.post(z == 0)
    x.getValue() should be(0)
    y.getValue() should be(1)
    z.getValue() should be(0)

  }
  
  test("TableAC5 Test 3") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarInt(cp, 1 to 7))
    
    val tuples = Array((1,1,1),(1,2,3),(1,2,7),(2,1,4))
    

    
    
    var nbSol = 0
    	
    cp.solveAll subjectTo {
      cp.add(new TableAC5TCRecomp(x(0),x(1),x(2),tuples))
    } exploration {
      cp.binary(x)
      nbSol += 1
    }
    nbSol should be(4)

  }
  
  test("TableAC5 Test 4") {
    val cp = CPSolver()
    var x = Array.fill(2)(CPVarInt(cp, 1 to 1))
    
    val tuples = Array((1,2),(2,1))
    

    cp.post(new TableAC5TCRecomp(x(0),x(1),tuples))
    cp.isFailed should be(true)
    

  }
  
  test("TableAC5 Test 5") {
    
    def nbSol(newcons: Boolean) = {
      val cp = CPSolver()
      val x = Array.fill(4)(CPVarInt(cp, Set(1,3,6,9)))
    
      val tuples = Array((1,2,2,4),
                         (1,2,4,8),
                         (1,1,9,6),
                         (1,1,8,6),
                         (3,1,6,9),
                         (1,9,3,1),
                         (1,9,9,9),
                         (3,6,6,6))
    
      val cons = if (newcons) new TableAC5TCRecomp(x(0),x(1),x(2),x(3),tuples) else new TableSTR2(x,tuples.map(_.productIterator.asInstanceOf[Iterator[Int]].toArray))
      cp.post(cons)
      var nbSol = 0
      cp.exploration {
        cp.binaryFirstFail(x)
        nbSol += 1
      }
      nbSol
    }
    nbSol(false) should be(nbSol(true))

  }
  
  
}
