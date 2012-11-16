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

import oscar.cp.mem.tsp.TSPParser.parseCoordinates
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._
import oscar.cp.constraints._
import oscar.cp.mem.visu.VisualRelax
import scala.collection.mutable.Queue
import scala.util.Random.nextInt
import scala.math.round
import oscar.search.IDSSearchController
import oscar.cp.mem.ACElement
import oscar.cp.mem.RoutingUtils

object myTSP extends App {

  // Data parsing
  // ------------
  val coord = parseCoordinates("data/TSP/kroA100.tsp")
  val rand = new scala.util.Random(0)

  // Random coordinates
  //val coord = Array.tabulate(20)(i => (100 + rand.nextInt(400), rand.nextInt(400)))

  
  val nCities = coord.size
  val Cities = 0 until nCities

  // Computes the distance between two cities
  def getDist(p1: (Int, Int), p2: (Int, Int)): Double = {
    val dx = p2._1 - p1._1
    val dy = p2._2 - p1._2
    math.sqrt(dx * dx + dy * dy)
  }

  // Builds the distance matrix
  val realDistMatrix = Array.tabulate(nCities, nCities)((i, j) => getDist(coord(i), coord(j)))
  val distMatrix = realDistMatrix.map(_.map(round(_).toInt))

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

  // LNS
  // ---
  case class Sol(pred: Array[Int], succ: Array[Int], dist: Int)

  var currentSol: Sol = null

  var nRestart = 1
  var nStagnation = 0
  var stagnation = false

  val pMin = 20
  val pMax = 60
  var p = pMin

  var firstLns = true

  cp.lns(500, 2000) {

    nRestart += 1

    if (firstLns) {
      println("Start LNS")
      firstLns = false
    }

    handleStagnation()

    relaxVariables(clusterRelax(p))
  }

  def handleStagnation() {

    if (stagnation) nStagnation += 1
    else {
      stagnation = true
      nStagnation = 0
      p = pMin + (p - pMin) / 2
    }

    if (nStagnation == 100) {
      nStagnation = 0
      if (p < pMax) p += 1
    }
  }

  def clusterRelax(p: Int): Array[Boolean] = {

    val c = nextInt(nCities)
    val sortedByDist = Cities.sortBy(i => distMatrix(c)(i))
    val dist = distMatrix(c)(sortedByDist(p))

    Array.tabulate(nCities)(i => distMatrix(c)(i) <= dist)
  }

  def solFound() = {
    stagnation = false
    currentSol = new Sol(pred.map(_.value), succ.map(_.value), totDist.value)
    visu.updateRoute(currentSol.pred)
    visu.updateDist()
  }

  def relaxVariables(selected: Array[Boolean]) {
    
    visu.updateSelected(selected)
    visu.updateRestart(nRestart)
    
    val constraints: Queue[Constraint] = Queue()
    
    for (c <- Cities; if !selected(c)) {
      if (!selected(currentSol.pred(c)))
        constraints enqueue (pred(c) == currentSol.pred(c))
      if (!selected(currentSol.succ(c)))
        constraints enqueue (succ(c) == currentSol.succ(c))
    }
    cp.post(constraints.toArray)
  }

  // Constraints
  // -----------
  cp.minimize(totDist) subjectTo {

    // Channeling between predecessors and successors
    //cp.add(new ChannelingPredSucc(cp, pred, succ))
    
    for (i <- Cities) {
      cp.add(ACElement(pred, succ(i)) == i)
      cp.add(ACElement(succ, pred(i)) == i)
    }

    // Consistency of the circuit with Strong filtering
    cp.add(circuit(succ), Strong)
    cp.add(circuit(pred), Strong)

    // Total distance
    cp.add(sum(Cities)(i => distMatrix(i)(succ(i))) == totDist)
    cp.add(sum(Cities)(i => distMatrix(i)(pred(i))) == totDist)

    cp.add(new TONOTCOMMIT(cp, pred, distMatrix, totDist))
    cp.add(new TONOTCOMMIT(cp, succ, distMatrix, totDist))
  }

  // Search
  // ------
  println("Searching...")
  cp.exploration {
    RoutingUtils.regretHeuristic(cp, succ, distMatrix)
    solFound()
  }

  cp.printStats()
}
