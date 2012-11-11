/** *****************************************************************************
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
  * ****************************************************************************
  */

package oscar.cp.test

import oscar.cp.mem.pareto.ParetoPoint

import org.scalacheck._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class TestParetoSet extends FunSuite with ShouldMatchers {
	
	/*trait Data {
		
		type Sol = Array[Int]
		
		val pareto = ParetoSet[Sol](2)
		
		val obj1 = Array(0, 4)
		val obj2 = Array(2, 2)
		val obj3 = Array(4, 0)
		
		val obj4 = Array(1, 2)
		val obj5 = Array(3, 0)
		
		val obj6 = Array(0, 0)
	}
	
		
	test("size") {
		new Data {
			
			pareto.size should be(0)
			pareto insert (obj1, obj1)
			pareto.size should be(1)
			pareto insert (obj2, obj2)
			pareto.size should be(2)
		}
	}
	
	test("insert 1 : singleton") {
		new Data {
			
			pareto insert (obj1, obj1) should be(true)
			pareto.size should be(1)
			
			val p = pareto.currentPoint
			pareto.nextPoint should be(p)
			pareto.currentPoint should be(p)
		}	
	}
	
	test("insert 2 : three pareto equivalent points") {
		new Data {
			
			pareto insert (obj1, obj1) should be(true)
			pareto insert (obj2, obj2) should be(true)
			pareto insert (obj3, obj3) should be(true)
			pareto.size should be(3)
		}		
	}
	
	test("insert 3 : dominating points") {
		new Data {
			
			pareto insert (obj2, obj2) should be(true)
			pareto.size should be(1)
			pareto insert (obj4, obj4) should be(true)
			pareto.size should be(1)
			
			pareto.currentSol should be(obj4)
		}		
	}
	
	test("insert 4 : dominating points 2") {
		new Data {
			
			pareto insert (obj1, obj1) should be(true)
			pareto.size should be(1)
			pareto insert (obj2, obj2) should be(true)
			pareto.size should be(2)
			pareto insert (obj3, obj3) should be(true)
			pareto.size should be(3)
			
			pareto insert (obj4, obj4) should be(true)
			pareto.size should be(3)
			pareto insert (obj5, obj5) should be(true)
			pareto.size should be(3)
			
			pareto insert (obj6, obj6) should be(true)
			pareto.size should be(1)
			
			pareto.currentSol should be(obj6)
		}		
	}
	
	test("insert 5 : not dominating points") {
		new Data {
			
			pareto insert (obj6, obj6) should be(true)
			pareto.size should be(1)
			pareto insert (obj4, obj4) should be(false)
			pareto.size should be(1)
			
			pareto.currentSol should be(obj6)
		}		
	}*/
}
