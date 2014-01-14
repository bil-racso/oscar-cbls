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
import collection.immutable.SortedSet



class TestMagicSquare extends FunSuite with ShouldMatchers  {


  test("MagicSquare") {
      
    val cp = CPSolver()
    
    val n = 3
    
    val x = Array.fill(n,n)(CPVarInt(1 to n*n)(cp))
     
    val s = ( n * (n*n + 1)) / 2;
		
	val diag1 = Array.tabulate(n)(i => x(i)(i))
	val diag2 = Array.tabulate(n)(i => x(i)(n-i-1))
	
	var nbSol = 0
	
	cp.solve subjectTo {
	  cp.add(allDifferent(x.flatten),Weak)
	  cp.add(sum(diag1) == s)
	  cp.add(sum(diag2) == s)
	  for (i <- 0 until n) {
		cp.add(sum(0 until n)(j => x(i)(j)) == s)
		cp.add(sum(0 until n)(j => x(j)(i)) == s)
	  }
	} search {
	  binaryFirstFail(x.flatten.toSeq)
	}
	
	cp.start().nSols should be(8)
	

    
  }  
  

  


}
