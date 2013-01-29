/**
 * *****************************************************************************
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

package oscar.invariants.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.collection.immutable._
import oscar.invariants._

class OccuringTest extends FunSuite with ShouldMatchers {

  test("Events and dispose") {

    var result = ""

    val e1 = Event[Int]()
    val e2 = Event[String]()

    val r1 = for (v <- e1) {
      result += v
      true
    }
    val r2 = for (v <- e2) {
      result += v
      true
    }

    val r3 = for (v <- e1) {
      result += 5 * v
      v < 30
    }

    e1 emit 1
    e1 emit 3
    e2 emit "iuiu"
    e1 emit 44
    e2 emit "ooo"
    e1 emit 0

    result should equal("15315iuiu44220ooo0")

    r2.dispose

    e1 emit 5
    e2 emit "yyy"
    e2 emit "rrr"
    e1 emit 9

    result should equal("15315iuiu44220ooo059")
  }

  test("filter on events") {
    val e1 = Event[Int]()

    var result = ""
    for (v <- e1; if v > 30) {
      result += v
      true
    }

    e1 emit 1
    e1 emit 4
    e1 emit 55
    e1 emit 23
    e1 emit 30
    e1 emit 44
    e1 emit 2

    result should equal("5544")
  }

  test("=== on events") {
    val e = Event[Int]()

    val e2 = e === 3
    var result = ""
    for (v <- e) {
      result += v
      true
    }

    for (v <- e2) {
      result += 4 * v
      true
    }

    e emit 4
    e emit 6
    e emit 8
    e emit 3
    e emit 1
    e emit 3
    e emit 3
    e emit 4

    result should equal("46831213123124")
  }

  test("whenever") {
    val e = Event[Int]()
    var result = ""
    var result2 = ""

    for (v <- e) {
      result += v
      true
    }

    whenever(e) { v =>
      result2 += v
    }

    result should equal(result2)
  }

  test("once") {
    val e = Event[String]()
    var result = ""
    once(e) {
      result += "zz"
    }

    e emit "oo"
    e emit "ii"

    result should equal("zz")

    once(e === "mm") {
      result += "ok"
    }
    result should equal("zz")

    e emit "mm"
    result should equal("zzok")
    e emit "mm"
    result should equal("zzok")

  }

  test("until") {
    val e = Event[Int]()
    val e2 = Event[Unit]()
    var result = ""

    val r = whenever(e) { v =>
      result += v
      true
    }
    
    e emit 1
    
    r until e2
    
    e emit 55
    e emit 99
    
    e2 emit ()
    
    e emit 66
    
    result should equal("15599")
    
    r dispose
    
    e emit 44
    result should equal("15599")
    
  }
}