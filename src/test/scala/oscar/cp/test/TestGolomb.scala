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
import collection.immutable.SortedSet


class TestGolomb extends FunSuite with ShouldMatchers  {


  test("Golomb Ruler") {
    // return the best ruler with n ticks
    def test(n: Int) = {
 
	  val cp = CPSolver()
	  
	  val marks = Array.fill(n)(CPVarInt(cp,0 to n*n))
	  
	  
	  val obj = marks(n-1)
	  var best = Int.MaxValue
	  
	  cp.minimize(obj) subjectTo {
          // we break symmetries to put the marks increasing
    	  cp.add(marks(0) == 0)
		  for (i <- 0 until n-1) {
			cp.add(marks(i) < marks(i+1))
		  }
    	  

          cp.add(allDifferent(for (i <- 0 until n; j <- i+1 until n) yield marks(j)-marks(i)), Strong);
                
                
          // break the symmetries between differences
          cp.add(marks(1)-marks(0) < marks(n-1)-marks(n-2));
    	  
    	  
        
      } exploration {
        cp.binaryFirstFail(marks)
        best = obj.value
      } run()
      best
    }
    
    test(6) should be(17)
    test(7) should be(25)
    
  }  
  

  


}
