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

import oscar.cp.mem.ParetoFront.ParetoPoint
import oscar.cp.mem.ParetoFront.ParetoSet

import org.scalacheck._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class TestParetoSet extends FunSuite with ShouldMatchers {
	
	trait TestSet {
		
		val s = ParetoSet(2)
		
		val p1 = ParetoPoint(2)
		p1(0) = 1
		p1(1) = 2
		
		val p2 = ParetoPoint(2)
		p2(0) = 2
		p2(1) = 1

		val p3 = ParetoPoint(2)
		p3(0) = 2
		p3(1) = 2
	}
	
	test("checkPoint 1") {
		
		val s = ParetoSet(2)
		val p = ParetoPoint(2)
		
		s checkPoint p should be(true)
	}
	
	test("checkPoint 2") {
		
		val s = ParetoSet(3)
		val p = ParetoPoint(2)
		
		val thrown = intercept[Exception] {
			s checkPoint p
		}
		
		thrown.getMessage should be("The dimension of the point does not fit the dimension of the set")
	}

	test("add 1 : non dominated points") {
		
		new TestSet {
			
			s add p1
			s.set.head should be(p1)
			
			s add p2
			s.set.tail should be(p2)
			
			s.size should be(2)
		}
	}
	
	test("add 2 : dominated points") {
		
		new TestSet {
			
			s add p1
			s add p2
			
			s add p3
			
			s.size should be(1)
			s.set.head should be(p3)
		}
	}
	
	test("nextPoint") {
		
		new TestSet {
			
			s add p1
			s add p2			
			s.size should be(2)
			
			s.nextPoint should be(p1)
			s.size should be(2)
			
			s.nextPoint should be(p2)
			s.size should be(2)
			
			// Test cycle
			s.nextPoint should be(p1)
			s.size should be(2)
		}
	}
}
