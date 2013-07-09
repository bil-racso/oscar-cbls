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
import oscar.cp.core._
import oscar.cp.modeling._
import oscar.cp.constraints.ElementCst2D

class TestGardnerPuzzle extends FunSuite with ShouldMatchers {
  
	 /**
	 * What is the minimum possible product of three different numbers in the set
	 * { -8, -6, -4, 0, 3, 5, 7 }?
	 * a) -336 b) -280 c) -210 d) -192 e) 0
	 */
	test("Minimum Product of 3 Elements") {
	  val cp = CPSolver()
	  val set = Array(-8, -6, -4, 0, 3, 5, 7)
	  val x = Array.fill(3)(CPVarInt(cp, 0  until set.length))
	  val obj = CPVarInt(cp, Array(-336, -280, -210, -192, 0))
	  var best = obj.getMax
	  cp.minimize(obj) subjectTo {
		   cp.add(obj == set(x(0)) * set(x(1)) * set(x(2)), CPPropagStrength.Strong)
		   cp.add(allDifferent(x), CPPropagStrength.Strong)
		   cp.add(x(0) < x(1))
		   cp.add(x(1) < x(2))
	  } exploration {
    	 cp.binary(x)
    	 best = obj.value
	  } run()
	  cp.printStats
	  best should be(-280)
	}
	
	
	/**
	 * The number 64 has the property that it is divisible by its units
	 * digit (e.g. 64 % 4 = 0, 42 % 2 = 0, etc.). 
	 * How many whole numbers between 10 and 50 have this property?
	 * a) 15 b) 16 c) 17 d) 18 e) 20 
	 */
	ignore("Numbers between 10 and 50 divisible unit digit")  {
		val cp = CPSolver()
		val R = (10 to 50)
		val obj = CPVarInt(cp, Array(15, 16, 17, 18, 20))
		val nos = Array.tabulate(R.size)(r => CPVarInt(cp, r))
		val units = Array.tabulate(R.size)(r => r % 10)
		
		
		units.foreach(println)
		
		val h = units.zipWithIndex.filter(_._1 != 0).map(_._2)
		h.foreach(println)
		/*
		cp.solve subjectTo {
			sum(nos)(n => )
		} exploration {
		  
		} run()*/
	}
}