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


class TestMultiplication extends FunSuite with ShouldMatchers  {


  test("Multiplication 1") {
    val cp = CPSolver()

    val x = CPVarInt(cp,-10 to 10);
    val y = CPVarInt(cp,Set(-70,-50,50,70))
    val z = CPVarInt(cp,100 to 100);
    	
    cp.post(new oscar.cp.constraints.MulVar(x,y,z)); // should post a MulCteRes because z is fixed
    	
    var nbSol = 0
    cp.exploration {
      cp.binaryFirstFail(Array(x,y))
      ((x.isBoundTo(-2) && y.isBoundTo(-50)) || (x.isBoundTo(2) && y.isBoundTo(50))) should be(true)
      nbSol += 1
    } run()
    nbSol should be(2)

  }
  
  test("Multiplication 2") {
    val cp = CPSolver()

    val x = CPVarInt(cp,-1 to 0)
    val y = CPVarInt(cp,0 to 1)
    cp.post(x*x == y)
    	
    cp.isFailed should be(false)

  }
  
  test("Multiplication 3") {
    val cp = CPSolver()

    val x = CPVarInt(cp,-10 to -1)
    val y = CPVarInt(cp,3 to 9)
    cp.post(x*x == y)
    	
    cp.isFailed should be(false)

    x.min should be (-3)
    x.max should be (-2)
    
  }  
  
  test("Multiplication 4: Guess the number") {
    val cp = CPSolver()
    

    val digits = Array.fill(5)(CPVarInt(cp,0 to 9))
    
    // with a one after (larger one)
    val nb1 =  digits(0)*100000 + digits(1)*10000 + digits(2)*1000 +  digits(3)*100 + digits(4)*10 + 1
    // with a one before (smaller one)
    val nb2 =  CPVarInt(cp,100000) + digits(0)*10000 + digits(1)*1000 +  digits(2)*100 + digits(3)*10 + digits(4)
    var nbsol = 0
    cp.solve subjectTo {
      cp.add(nb1 == (nb2*3))
    } exploration {
      cp.binary(digits)
      nbsol += 1
      nb1.value should be(428571)
      nb2.value should be(142857)
    } run()
    nbsol should be(1)
    
    
  }    
  

}
