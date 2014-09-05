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
package oscar.linprog.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.linprog.modeling._
import oscar.algebra._

/**
 * Test functionality of Algebra related to linear programming and the correct domain creation of variables
 */
class AlgebraTest extends FunSuite with ShouldMatchers {

	implicit val lp = new LPSolver()
  
	val x1 = LPFloatVar("x1",3.5,100)
	val x2 = LPFloatVar("x2")
	val x3 = LPFloatVar("x3",true)
	val x4 = LPFloatVar("x4",false)
	
	val mip = new MIPSolver()
  
	val y1 = new MIPFloatVar(mip,"y1",3.5,100)
	val y2 = new MIPFloatVar(mip,"y2")
	val y3 = new MIPFloatVar(mip,"y3",true)
	val y4 = new MIPFloatVar(mip,"y4",false)	
	
  test("Normalization, simplification and equality of linear expression") {
	  	val expr0 = -2 * x3 -2 * x1 - x2 + x3 + x1 -1 + 1* x3 // equal -x1 -x2 -1
	  	expr0.equals(-x1-x2-1) should equal (true)
		  
	  	val expr1 = (1 -(expr0 - 1) +2 - 1) // 4 + x1 + x2
	    expr1.equals(4+x1+x2) should equal (true)
		  
	  	val expr2 = -expr1 // -4 - x1 - x2
	    expr2.equals(-4-x1-x2) should equal (true)
	  	expr2.equals(-4-sum(List(x1,x2)))
  }
	
  test("The domains of variables of LP") {
	x1.lb should equal (3.5)
	x1.ub should equal (100)
	x1.unbounded should equal (false)

	x2.lb should equal (0)
	x2.unbounded should equal (false)	
	
	x3.unbounded should equal (true)
    
	x4.lb should equal (0)
	x4.unbounded should equal (false)
  }
  
  test("The domains of variables of MIP") {
	y1.lb should equal (3.5)
	y1.ub should equal (100)
	y1.unbounded should equal (false)

	y2.lb should equal (0)
	y2.unbounded should equal (false)	

	y3.unbounded should equal (true)
    
	y4.lb should equal (0)
	y4.unbounded should equal (false)
  } 
  
  // TODO This test case doesn't have any assertions yet (so isn't testing anything)
  test("Large sum") {
	    val lp = new LPSolver()
	    lp.name = "Large Sum"
	    val x = Array.tabulate(100000)(i => new LPFloatVar(lp, i.toString(), 0, 1))
	    val startTime = System.currentTimeMillis()
	    val mysum = sum(x)
	    println(s"Sum of ${x.size} variables took ${System.currentTimeMillis() - startTime} ms to calculate")
  }     
}

