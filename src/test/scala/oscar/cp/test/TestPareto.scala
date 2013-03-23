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
import oscar.cp.mem.pareto.ListPareto


class TestPareto extends FunSuite with ShouldMatchers  {
  
  
  test("pareto1") { 
	  val p1 = new ListPareto[Any](Array(true,true))
	  val p2 = new ListPareto[Any](Array(true,true))
	  
	  p1 == p2 should be(true)
	  p1.insert(null, 1,2)
	  p1 == p2 should be(false)
	  p2.insert(null, 1,2)
	  p1 == p2 should be(true)
	  p1.insert(null, 0,3)
	  p1 == p2 should be(false)
	  p2.insert(null, 0,3)
	  p1 == p2 should be(true)
  }  
  

 


}
