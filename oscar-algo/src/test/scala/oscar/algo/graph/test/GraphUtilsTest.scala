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
package oscar.algo.graph.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.algo.graph.GraphUtils

/**
 * Created on 06/03/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */

class GraphUtilsTest extends FunSuite with Matchers {

  test("Test of Dijkstra's shortest path algorithm ") {
    // The example was found at http://i.stack.imgur.com/YC8LA.gif
    val edgeCosts = Array(
      Array(0, 2, Int.MaxValue, 1, Int.MaxValue, Int.MaxValue, Int.MaxValue),
      Array(Int.MaxValue, 0, Int.MaxValue, 3, 10, Int.MaxValue, Int.MaxValue),
      Array(4, Int.MaxValue, 0, Int.MaxValue, Int.MaxValue, 5, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, 2, 0, 2, 8, 4),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, Int.MaxValue, 6),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 0, Int.MaxValue),
      Array(Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, Int.MaxValue, 1, 0)
    )

    val shortestPaths = GraphUtils.dijkstra(0, edgeCosts)
    shortestPaths(0)._1 shouldBe Array(0)
    shortestPaths(0)._2 shouldBe 0
    shortestPaths(1)._1 shouldBe Array(0, 1)
    shortestPaths(1)._2 shouldBe 2
    shortestPaths(2)._1 shouldBe Array(0, 3, 2)
    shortestPaths(2)._2 shouldBe 3
    shortestPaths(3)._1 shouldBe Array(0, 3)
    shortestPaths(3)._2 shouldBe 1
    shortestPaths(4)._1 shouldBe Array(0, 3, 4)
    shortestPaths(4)._2 shouldBe 3
    shortestPaths(5)._1 shouldBe Array(0, 3, 6, 5)
    shortestPaths(5)._2 shouldBe 6
    shortestPaths(6)._1 shouldBe Array(0, 3, 6)
    shortestPaths(6)._2 shouldBe 5
  }
}

