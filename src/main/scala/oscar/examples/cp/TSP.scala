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

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._

import scala.io.Source

/**
 * Traveling Salesman Problem
 *
 * Given a distance matrix between 20 cities,
 * find the shortest tour visiting each city exactly once.
 *
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object TSP extends App {

  val nCities = 20
  val Cities  = 0 until nCities

  // Data parsing
  // ------------
  val lines = Source.fromFile("data/tsp.txt").getLines.toList
  val distMatrix = lines.grouped(nCities).map(i => i.map(j => j.toInt).toArray).toArray

  // Model
  // -----
  val cp = new CPSolver()

  // Successors
  val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
  // Predecessors
  val pred = Array.fill(nCities)(CPVarInt(cp, Cities))
  // Total distance
  val totDist = CPVarInt(cp, 0 to distMatrix.flatten.sum)

  // Constraints + Search
  // --------------------
  cp.minimize(totDist) subjectTo {

    // Channeling between predecessors and successors
    for (i <- Cities) {
      cp.add(pred(succ(i)) == i)
      cp.add(succ(pred(i)) == i)
    }

    // Consistency of the circuit with Strong filtering
    cp.add(circuit(succ), Strong)
    cp.add(circuit(pred), Strong)

    // Total distance
    cp.add(sum(Cities)(i => distMatrix(i)(succ(i))) == totDist)
    cp.add(sum(Cities)(i => distMatrix(i)(pred(i))) == totDist)

  } exploration {

    // Greedy heuristic
    while (!allBounds(succ)) {

      // Select the not yet bound city with the smallest number of possible successors
      val x = selectMin(Cities)(!succ(_).isBound)(succ(_).size).get
      // Select the closest successors of the city x
      val v = selectMin(Cities)(succ(x).hasValue(_))(distMatrix(x)(_)).get

      cp.branch(cp.post(succ(x) == v))(cp.post(succ(x) != v))
    }
    
    //println("best sol")
  }

  cp.printStats()
}
