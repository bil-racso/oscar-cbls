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

package oscar.cp.mem.tsp

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._
import oscar.cp.constraints._
import oscar.cp.mem.visu.VisualRelax


object myTSP extends App {

  val scale = 100
  val nCities = 200
  val Cities = 0 until nCities

  // Data parsing
  // ------------
  val rand = new scala.util.Random(0)

  // Random coordinates
  val coord = Array.tabulate(nCities)(i => (rand.nextInt(500), rand.nextInt(500)))

  // Computes the distance between two cities
  def getDist(p1: (Int, Int), p2: (Int, Int)): Double = {
    val dx = p2._1 - p1._1
    val dy = p2._2 - p1._2
    math.sqrt(dx * dx + dy * dy)
  }

  // Builds the distance matrix
  val distMatrix = Array.tabulate(nCities, nCities)((i, j) => (getDist(coord(i), coord(j))*scale).toInt)
  val realDistMatrix = Array.tabulate(nCities, nCities)((i, j) => getDist(coord(i), coord(j)))

  // Model
  // -----
  val cp = new CPSolver()

  // Successors
  val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
  // Predecessors
  val pred = Array.fill(nCities)(CPVarInt(cp, Cities))
  // Total distance
  val totDist = CPVarInt(cp, 0 to distMatrix.flatten.sum)
  
      
  // Visualization
  // -------------
  val visu = new VisualRelax(coord, realDistMatrix)

  // Constraints + Search
  // --------------------
  var nSol = 0
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
    
    cp.add(new TONOTCOMMIT(cp, pred, distMatrix, totDist))
    cp.add(new TONOTCOMMIT(cp, succ, distMatrix, totDist))

  } exploration {

    // Greedy heuristic
    while (!allBounds(succ)) {

      // Selects the not yet bound city with the smallest number of possible successors
      val x = selectMin(Cities)(!succ(_).isBound)(succ(_).size).get
      // Selects the closest successors of the city x
      val v = selectMin(Cities)(succ(x).hasValue(_))(distMatrix(x)(_)).get

      cp.branch(cp.post(succ(x) == v))(cp.post(succ(x) != v))
    }
    
    nSol += 1
    
    visu.updateDist
    visu.updateRoute(pred.map(_.value))
    
    if (nSol == 40) cp.stop
  }

  cp.printStats()
}
