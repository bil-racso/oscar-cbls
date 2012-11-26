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
import oscar.cp.mem.RoutingUtils
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.MyCircuit

object myTSP extends App {

  // Data parsing
  // ------------
  val coord = parseCoordinates("data/TSP/kroB100.tsp")
  val rand = new scala.util.Random(0)

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
  val distMatrix = realDistMatrix.map(_.map(i => round(i*10).toInt))

  // Model
  // -----
  val cp = new CPSolver()
  
  // Successors
  //val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
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
  var nStarts = 1
  var p = 15

  cp.lns(500, 500) {
       
    nStarts += 1
    if (nStarts == 2) {
      println("Start LNS")
      cp.failLimit = 2004310016 // 15! : UB on #possibilities
    }

    relaxVariables(pathRelax(p))
  }

  def randomRelax(p: Int): Array[Boolean] = {
    
    val c = nextInt(nCities)
    val selected = Array.fill(nCities)(false)
    selected(c) = true

    for (i <- 1 until p) {
      val rem = Cities.filter(i => !selected(i))
      val c = rem(nextInt(rem.size))
      selected(c) = true
    }
    
    selected
  }
  
  def clusterRelax(p: Int): Array[Boolean] = {

    val c = nextInt(nCities)
    val sortedByDist = Cities.sortBy(i => distMatrix(c)(i))
    val dist = distMatrix(c)(sortedByDist(p))

    Array.tabulate(nCities)(i => distMatrix(c)(i) <= dist)
  }

  def pathRelax(p: Int): Array[Boolean] = {

    val c = nextInt(nCities)
    val selected = Array.fill(nCities)(false)
    selected(c) = true

    for (i <- 1 until p) {

      val sel = Cities.filter(i => selected(i))
      val rem = Cities.filter(i => !selected(i))

      val c = sel(nextInt(sel.size))
      val cc = rem.sortBy(i => distMatrix(c)(i)).head

      selected(cc) = true
    }
    selected
  }

  def solFound() = {
    //currentSol = new Sol(pred.map(_.value), succ.map(_.value), totDist.value)
    currentSol = new Sol(pred.map(_.value), pred.map(_.value), totDist.value)
    visu.updateRoute(currentSol.pred)
    visu.updateDist()
  }

  def relaxVariables(selected: Array[Boolean]) {

    visu.updateSelected(selected)
    visu.updateRestart(nStarts)

    val constraints: Queue[Constraint] = Queue()

    for (c <- Cities; if !selected(c)) {
      if (!selected(currentSol.pred(c)))
        constraints enqueue (pred(c) == currentSol.pred(c))
      //if (!selected(currentSol.succ(c)))
        //constraints enqueue (succ(c) == currentSol.succ(c))
    }
    cp.post(constraints.toArray)
  }

  // Constraints
  // -----------
  cp.minimize(totDist) subjectTo {

    // Channeling between predecessors and successors
    //cp.add(new ChannelingPredSucc(cp, pred, succ))

    // Consistency of the circuit with Strong filtering
    //cp.add(circuit(succ), Strong)
    //cp.add(circuit(pred), Strong)
    
    //cp.add(new MyCircuit(cp, succ), Strong)
    cp.add(new MyCircuit(cp, pred), Strong)

    // Total distance
    //cp.add(sum(Cities)(i => distMatrix(i)(succ(i))) == totDist)
    cp.add(sum(Cities)(i => distMatrix(i)(pred(i))) == totDist)

    //cp.add(new TONOTCOMMIT(cp, pred, distMatrix, totDist))
    //cp.add(new TONOTCOMMIT(cp, succ, distMatrix, totDist))
  }

  // Search
  // ------
  println("Searching...")
  cp.exploration {
    
    RoutingUtils.regretHeuristic(cp, pred, distMatrix)
    //RoutingUtils.minDomDistHeuristic(cp, pred, succ, distMatrix)
    solFound()
  }

  cp.printStats()
}
