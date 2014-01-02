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


class TestSum extends FunSuite with ShouldMatchers  {
  
  
  test("sum0") { 	
	  	
	  	val s = CPSolver()
	  	
    	val x0 = CPVarInt(0,1)(s)
    	val x1 = CPVarInt(0,2)(s)
    	val x2 = CPVarInt(0,2)(s)
    	val x3 = CPVarInt(0,3)(s)
    	
    	val  cumulatedCounters = Array(x0,x1,x2,x3)
    	val n = cumulatedCounters.size
    	val len = 2
    	val min = 1
    	val max = 2
    	
    	// 0-1 , 0-2 , 0-2 , 0-3

        for (i <- 0 to n-len) {
            var nbVal = cumulatedCounters(i+len-1)
            if (i > 0) {
            	nbVal = nbVal - cumulatedCounters(i-1)
            }
            s.post(nbVal <= max) should not be(CPOutcome.Failure)
            s.post(nbVal >= min) should not be(CPOutcome.Failure)
            s.isFailed() should be(false)  
        }
    }
  
  test("sum1") { 
	  val cp = CPSolver()
	  val x = Array(CPVarInt(0 to 5)(cp), CPVarInt(1 to 5)(cp), CPVarInt(0 to 5)(cp))
	  val y = CPVarInt(0 to 100)(cp)
	  cp.add(sum(x,y))
	  y.min should be(1)
	  y.max should be(15) 
  } 
  
  test("sum2") { 
	  val cp = CPSolver()
	  val x = Array(CPVarInt(-5 to 5)(cp), CPVarInt(1 to 2)(cp), CPVarInt(0 to 1)(cp))
	  val y = CPVarInt(0 to 100)(cp)
	  cp.add(sum(x,y))
	  x(0).min should be(-3)
	  y.min should be(0) 
	  y.max should be(8) 
  }
  
  test("sum3") { 
	  val cp = CPSolver()
	  val x = Array(CPVarInt(0 to 5)(cp), CPVarInt(0 to 5)(cp), CPVarInt(0 to 5)(cp))
	  val y = CPVarInt(5)(cp)
	  cp.add(sum(x,y))
	  cp.add(x(1) == 0)
	  cp.add(x(0) >= 1)
	  x(0).max should be(5)
	  
	  x(2).min should be(0)
	  x(2).max should be(4)  
  }
  
  
  val rand = new scala.util.Random(0)
  def solve(x: Array[CPVarInt], y: CPVarInt, decomp: Boolean = false): Int = {
    val cp = y.s.asInstanceOf[CPSolver]
    //cp.pushState()
    var nbSol = 0
    cp.solve subjectTo {
      if (decomp) cp.add(new oscar.cp.constraints.implementations.Sum(x,y))
      else cp.add(sum(x,y))
    } exploration {
      while(!x.forall(_.isBound)) {
        val z = x.find(!_.isBound).get
        val v = z.min
        cp.branch
        {
          //println("banching on =="+v)
          cp.add(z == v)}
        {
          //println("branching on !="+v)
          cp.add(z!=v)}
        //println("branched z="+z)
      }
      //cp.binary(x :+ y, z => rand.nextInt(30))
      nbSol += 1
    } run()
    cp.popAll()
    nbSol
    
  }
  
  test("sum4") { 
    
	  val cp = CPSolver()
	  val x = Array(CPVarInt(-2 to 5,"x0")(cp), CPVarInt(-4 to 5,"x1")(cp), CPVarInt(3 to 5,"x2")(cp))
	  val y = CPVarInt(4 to 5,"y")(cp)
	  println("---------------")
	  println("=>"+solve(x,y,true))
	  println("=>"+solve(x,y))
	  println("----------------")
	  //solve(x,y,false) should be(solve(x,y,true))
  }
  
  test("sum5") { 
	  val cp = CPSolver()
	  val x = Array(CPVarInt(Set(-5,-3,2,8))(cp), CPVarInt(Set(-10,8))(cp), CPVarInt(3 to 5)(cp))
	  val y = CPVarInt(3 to 8)(cp)
	  
	  println("=>"+solve(x,y,true))
	  println("=>"+solve(x,y))
	  
	  solve(x,y,false) should be(solve(x,y,true))
  }
  
  test("sum6") { 
	  val cp = CPSolver()
	  val x = Array(CPVarInt(Set(-5,-3,2,8))(cp), CPVarInt(Set(-10,8))(cp), CPVarInt(3 to 5)(cp),CPVarInt(-10 to -5)(cp))
	  val y = CPVarInt(3 to 8)(cp)
	  
	  println("=>"+solve(x,y,true))
	  println("=>"+solve(x,y))
	  
	  solve(x,y,false) should be(solve(x,y,true))
  }   
   
  
  
  

}
