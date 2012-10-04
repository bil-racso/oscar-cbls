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

import oscar.cp.mem.paretoFront.ParetoPoint

import org.scalacheck._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class TestParetoPoint extends FunSuite with ShouldMatchers {

	test("apply") {
		
		val p = ParetoPoint(3)
		
		p(0) should be(0)
		p(1) should be(0)
		p(2) should be(0)
	}
	
	test("update") {
		
		val p = ParetoPoint(3)
		
		p(0) = 3
		p(1) = 2
		p(2) = 1
		
		p(0) should be(3)
		p(1) should be(2)
		p(2) should be(1)
	}

	test("isDominating 1") {
		
		val p1 = ParetoPoint(2)
		val p2 = ParetoPoint(2)
		
		p1(0) = 2
		p1(1) = 2
		
		p2(0) = 1
		p2(1) = 1
		
		p1 isDominating p2 should be(true)
	}
	
	test("isDominating 2") {
		
		val p1 = ParetoPoint(2)
		val p2 = ParetoPoint(2)
		
		p1(0) = 2
		p1(1) = 2
		
		p2(0) = 2
		p2(1) = 1
		
		p1 isDominating p2 should be(true)
	}
	
	test("isDominating 3") {
		
		val p1 = ParetoPoint(2)
		val p2 = ParetoPoint(2)
		
		p1(0) = 1
		p1(1) = 2
		
		p2(0) = 2
		p2(1) = 1
		
		p1 isDominating p2 should be(false)
	}
	
	test("isDominating 4") {
		
		val p1 = ParetoPoint(2)
		val p2 = ParetoPoint(2)
		
		p1(0) = 1
		p1(1) = 1
		
		p2(0) = 2
		p2(1) = 2
		
		p1 isDominating p2 should be(false)
	}
}
