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

import org.scalacheck._

class TestLogicalConstraints extends FunSuite with ShouldMatchers  {


  test("test logical 1") {
    
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarBool(cp))
    
    cp.post((x(0) && x(1)) || x(2))
    	
    cp.post(x(2) == 0)
    
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(0).value should be(1)
    x(1).value should be(1)

  }
  
  test("test logical 2") {
    
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarBool(cp))
    
    cp.post((x(0) && x(1)) || x(2))
    	
    cp.post(x(0) == 0)
    
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(2).value should be(1)
    x(1).isBound should be(false)

  }
  
  test("test logical 3") {
    
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarBool(cp))
    
    cp.post((x(0) && x(1)) && x(2))
  
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(0).value should be(1)
    x(1).value should be(1)
    x(2).value should be(1)
    
  }
  
  test("test logical 4") {
    
    val cp = CPSolver()
    
    val A = CPVarBool(cp)
	val B = CPVarBool(cp)
	val C = CPVarBool(cp)
	val D = CPVarBool(cp)
	

	cp.add(((A ==> B) || C) && D)
	
	D.isTrue should be(true)
    
	cp.add(A)
	cp.add(B == 0)
	
	C.isTrue should be(true)
    
  }
  
  test("test logical 5") {
    
    val cp = CPSolver()
    
    val w = CPVarInt(cp,1 to 5)
    val x = CPVarInt(cp,1 to 5)
 
    val A = w <<= x

	cp.add(A)
	
	w.max should be(4)
 
    cp.add(w <<= 2)
    w.max should be(1)
    
  }  
  
  test("test logical 6") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,1 to 5)
    val z = CPVarInt(cp,1 to 5)
   
    cp.add(z >>= y)
    z.min should be(2)
    
    cp.add((z >>= 3).constraintFalse()) // it means z <= 3
    z.max should be(3)
    
  }   
  
  test("test logical 7") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,1 to 5)
    val z = CPVarInt(cp,3 to 5)
    val b = z === y
    
    cp.add(y <= 2)
    b.value should be(0)
    
  } 
  
  test("test logical 8") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,Set(1,5,9))
    val z = CPVarInt(cp,Set(5,10,13))
    val b = z === y
    
    println(b)
    
    b.isBound should be(false)
    
    cp.add(z != 5)
    
    b.value should be(0)
    
  }
  
  test("test logical 9") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,Set(1,5,9))
    val z = CPVarInt(cp,Set(5,10,13))
    val b = z === y
    cp.add(b)
    
    y.value should be(5)
    z.value should be(5)
    
  }
  
  test("test logical 10") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,Set(1,5,9))
    val z = CPVarInt(cp,Set(5,10,13))
    val b = z === y
    cp.add(b.constraintFalse())
    cp.add(z == 5)
    y.hasValue(5) should be(false)
    
  }
  
  test("test logical 11") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,Set(1,5,9))
    val z = CPVarInt(cp,Set(5,10,13))
    val b = z !== y
    cp.add(b)
    cp.add(z == 5)
    y.hasValue(5) should be(false)
    
  } 
  
  test("test logical 12") {
    
    val cp = CPSolver()
    
    val x = CPVarBool(cp)
    val y = CPVarBool(cp)
    cp.add(x || y)
    cp.post(x === 0 && y === 0)
    cp.getStatus() should be (CPOutcome.Failure)
    
  }     
  

  

  

}
