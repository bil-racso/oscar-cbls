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

class TestSpread extends FunSuite with ShouldMatchers  {


  test("Spread 1") {
    val cp = CPSolver()
    var x = Array.fill(5)(CPVarInt(cp, 0 to 4))
    var s2 = CPVarInt(cp, 0 to 1000)
    cp.add(new Spread(x,10,s2))
    s2.min should be(20)
  }  
  
  test("Spread 2") {
    val cp = CPSolver()
    var x = Array.fill(5)(CPVarInt(cp, 0 to 4))
    var s2 = CPVarInt(cp, 0 to 1000)
    cp.add(new Spread(x,11,s2))
    s2.min should be(25)
  }
  
  test("Spread 3") {
    val cp = CPSolver()
    var x = Array(CPVarInt(cp, -1 to 0),
                  CPVarInt(cp, -2 to -1),
                  CPVarInt(cp, -3 to -2))
                       
    var s2 = CPVarInt(cp, 0 to 1000)
    cp.add(new Spread(x,-4,s2))
    
    s2.min should be(6)
  }
 
  test("Spread 4") {
    val cp = CPSolver()
    var x = Array(CPVarInt(cp, -1 to 0),
                  CPVarInt(cp, -3 to -2),
                  CPVarInt(cp, -3 to -2))
                       
    var s2 = CPVarInt(cp, 0 to 1000)
    cp.add(new Spread(x,-5,s2))
    s2.min should be(9)
  } 
  
  test("Spread 5") {
    val cp = CPSolver()
    
    var x = Array(CPVarInt(cp, 103),
                  CPVarInt(cp, 99),
                  CPVarInt(cp, 90 to 98),
                  CPVarInt(cp, 72 to 82),
                  CPVarInt(cp, 78),
                  CPVarInt(cp, 65),
                  CPVarInt(cp, 73))
                       
    var s2 = CPVarInt(cp, 0 to Int.MaxValue)
    cp.add(new Spread(x,591,s2))
    cp.isFailed should be(false)
  } 
  
  test("Spread 6") {
    val cp = CPSolver()
    
    // 4 vars, with dom 0 to 100
    // sum should be 8 (2 on average)
    
    var x = Array(CPVarInt(cp, 0 to 100),
                  CPVarInt(cp, 0 to 100),
                  CPVarInt(cp, 0 to 100),
                  CPVarInt(cp, 0 to 100))
                       
    var s2 = CPVarInt(cp, 0 to 16)
    cp.add(new Spread(x,8,s2))
    cp.isFailed should be(false)
    s2.min should be(16)
    for (y <- x) {
      y.max should be(2)
    }
  }
  
  test("Spread 7") {
    val cp = CPSolver()
            
    var x = Array(CPVarInt(cp, 0 to 0),
                  CPVarInt(cp, 0 to 100),
                  CPVarInt(cp, 0 to 100),
                  CPVarInt(cp, 0 to 100))
    // optimal assignment will be 0,2,3,3                   
    var s2 = CPVarInt(cp, 0 to 22)
    cp.add(new Spread(x,8,s2))
    cp.isFailed should be(false)
    s2.min should be(22)
    x(1).max should be(3)
    x(2).max should be(3)
    x(3).max should be(3)
    
  } 
  
  test("Spread 8") {
    val cp = CPSolver()
            
    var x = Array(CPVarInt(cp, 0 to 0),
                  CPVarInt(cp, 1 to 2),
                  CPVarInt(cp, 0 to 100),
                  CPVarInt(cp, 0 to 100))
    // optimal assignment will be 0,2,3,3                   
    var s2 = CPVarInt(cp, 0 to 22)
    cp.add(new Spread(x,8,s2))
    cp.isFailed should be(false)
    s2.min should be(22)
    x(1).min should be(2)
    x(2).min should be(3)
    x(3).min should be(3)
    x(2).max should be(3)
    x(3).max should be(3)
  }
  
  test("Spread 9") {
    val cp = CPSolver()
            
    var x = Array(CPVarInt(cp, 0 to 0),
                  CPVarInt(cp, 1 to 2),
                  CPVarInt(cp, 4 to 100),
                  CPVarInt(cp, 0 to 100))
    // optimal assignment will be 0,2,4,2                   
    var s2 = CPVarInt(cp, 0 to 24)
    cp.add(new Spread(x,8,s2))
    cp.isFailed should be(false)
    s2.min should be(24)
    x(1).min should be(2)
    x(2).min should be(4)
    x(3).min should be(2)
    x(2).max should be(4)
    x(3).max should be(2)
  }
  
  test("Spread 10") {
    val cp = CPSolver()
            
    var x = Array(CPVarInt(cp, 0 to 0),
                  CPVarInt(cp, 1 to 2),
                  CPVarInt(cp, 4 to 100),
                  CPVarInt(cp, 0 to 100))
    // optimal assignment will be 0,2,4,2 => let's allow this one 0,1,4,3 with s2=26                   
    var s2 = CPVarInt(cp, 0 to 26)
    cp.add(new Spread(x,8,s2))
    cp.isFailed should be(false)
    s2.min should be(24)
    x(1).min should be(1)
    x(2).max should be(4)
    x(3).max should be(3)
  }
    	
 

}
