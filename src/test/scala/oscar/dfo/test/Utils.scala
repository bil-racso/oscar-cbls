/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.dfo.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.dfo.algo._
import oscar.dfo.utils._

class IntervalTest extends FunSuite with ShouldMatchers {
  val testInt = Interval(-50.0, 50.0)
  
  test("size function") {
    testInt.size should equal (100.0)
  }
  
  test("getClosestBound(Double) function") {
	testInt.getClosestBound(100.0) should equal (50.0)
    testInt.getClosestBound(10.0) should equal (50.0)
    testInt.getClosestBound(-100.0) should equal (-50.0)
    testInt.getClosestBound(-10.0) should equal (-50.0)
    testInt.getClosestBound(0.0) should equal (-50.0)
  }
  
  test("isInInterval(Double) function") {
    testInt.isInInterval(100.0) should equal (false)
    testInt.isInInterval(0.0) should equal (true)
    testInt.isInInterval(-100.0) should equal (false)
  }
}

class QuasiRandomSequenceTest extends FunSuite with ShouldMatchers {
  val testQRS = new QuasiRandomSequence(new scala.util.Random(42))
  
  test("convertToInterval(Double, Interval) function") {
    testQRS.convertToInterval(0.25, Interval(2.0, 3.0)) should equal (2.25)
    testQRS.convertToInterval(0.2, Interval(-2.0, 3.0)) should equal (-1.0)
    testQRS.convertToInterval(0.25, Interval(-2.0, -1.0)) should equal (-1.75)
  }
  
  test("halton(Int, Int) function") {
    testQRS.halton(0, 2) should equal (0.5)
  }
  
  test("halton1D(Int, Interval, Int) function") {
    testQRS.halton1D(2, Interval(0, 1), 2) should equal (Array(0.5, 0.25))
  }
  
  test("haltonSequence(Int, Array[Interval]) function") {
    val dom = Array(Interval(0, 1), Interval(0, 1))
    testQRS.haltonSequence(2, dom) should equal (Array(Array(0.5, 1.0/3), Array(0.25, 2.0/3)))
  }
}
