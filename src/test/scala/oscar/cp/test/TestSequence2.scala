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


class TestSequence2 extends FunSuite with ShouldMatchers  {
  
  
  test("test1") { 
	  val cp = CPSolver()
	  val x = Array.fill(5)(CPVarInt(cp,Set(1,4,7)))
	  var nbSol = 0
	  cp.solve subjectTo { 
		cp.add(new SequenceDecomposition(x,Set(1,4),l=3,min=2,max=2))
	  } exploration {
	    cp.binaryFirstFail(x)
	    nbSol += 1
	  } run()
	  nbSol should be(32)
  }
  
  test("test2") { 
	  val cp = CPSolver()
	  val x = Array.fill(4)(CPVarInt(cp,0 to 1))
	  cp.add(new SequenceDecomposition(x,Set(1),l=2,min=1,max=2))
	  cp.add(x(2) == 0)
	  x(3).isBound should be(true)
	  x(1).isBound should be(true)
	  x(1).value should be(1)
	  x(3).value should be(1)
	  cp.isFailed should be(false)
  } 
  
  test("test3") { 
	  val cp = CPSolver()
	  val x = Array.fill(4)(CPVarInt(cp,1 to 5))
	  cp.add(new SequenceDecomposition(x,Set(3),l=2,min=1,max=2))
	  cp.add(x(2) == 2)
	  x(3).isBound should be(true)
	  x(1).isBound should be(true)
	  x(1).value should be(3)
	  x(3).value should be(3)
	  cp.isFailed should be(false)
  }
  
  test("test4") { 
	  val cp = CPSolver()
	  val x = Array.fill(4)(CPVarInt(cp,1 to 5))
	  cp.add(new SequenceDecomposition(x,Set(2,3),l=2,min=1,max=2))
	  cp.add(x(2) == 1)
	  x(1).toSet should be(Set(2,3))
	  x(3).toSet should be(Set(2,3))
	  cp.isFailed should be(false)
  }
  
  test("test5") { 
	  val cp = CPSolver()
	  val x = Array.fill(4)(CPVarInt(cp,1 to 5))
	  cp.add(new SequenceDecomposition(x,Set(4),l=1,min=1,max=1))
	  x.forall(_.isBoundTo(4)) should be(true)
   }
}


