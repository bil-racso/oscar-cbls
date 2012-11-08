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
import oscar.cp.mem.pareto.ParetoSet

import scala.collection.mutable.Queue
import scala.util.Random.nextInt
import scala.util.Random.nextFloat
import scala.math.round

object MO_TSP extends App {

  // Data parsing
  // ------------
  val coord1 = parseCoordinates("data/TSP/kroA100.tsp")
  val coord2 = parseCoordinates("data/TSP/kroB100.tsp")

  val nCities = coord1.size
  val Cities = 0 until nCities

  // Computes the distance between two cities
  def getDist(p1: (Int, Int), p2: (Int, Int)): Double = {
    val dx = p2._1 - p1._1
    val dy = p2._2 - p1._2
    math.sqrt(dx * dx + dy * dy)
  }

  // Builds the distance matrix
  val realDistMatrix1 = Array.tabulate(nCities, nCities)((i, j) => getDist(coord1(i), coord1(j)))
  val realDistMatrix2 = Array.tabulate(nCities, nCities)((i, j) => getDist(coord2(i), coord2(j)))

  val distMatrix1 = realDistMatrix1.map(_.map(round(_).toInt))
  val distMatrix2 = realDistMatrix2.map(_.map(round(_).toInt))

  // Model
  // -----
  val cp = new CPSolver()

  // Successors
  val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
  // Predecessors
  val pred = Array.fill(nCities)(CPVarInt(cp, Cities))
  // Total distance
  val totDist1 = CPVarInt(cp, 0 to distMatrix1.flatten.sum)
  val totDist2 = CPVarInt(cp, 0 to distMatrix2.flatten.sum)

  // Visualization
  // -------------
  val visu1 = new VisualRelax(coord1, realDistMatrix1)
  val visu2 = new VisualRelax(coord2, realDistMatrix2)

  // MOLNS
  // -----

  val nObjs = 2
  val Objs = 0 until nObjs
  val pareto = ParetoSet[Sol](nObjs)

  case class Sol(pred: Array[Int], succ: Array[Int], dist1: Int, dist2: Int)
  
  var currentSol: Sol = null

  var nRestart = 1
  var nObjRestart = 0
  var nStagnation = 0
  var newSol = true

  val pMin = 20
  val pMax = 60
  var p = pMin

  var firstLns = true

  cp.lns(500, 2000) {
    
    println("pareto size : "+ pareto.size)
    
    if (newSol) {
      newSol = false     
      val obj = Array(currentSol.dist1, currentSol.dist2)
      pareto.insert(obj, currentSol)
    }
    
    nRestart += 1

    if (firstLns) {
      println("Start LNS")
      firstLns = false
    }

    handleObjectives()   
    currentSol = pareto.currentSol
    
    visu1.updateRoute(currentSol.pred)
    visu1.updateDist()

    visu2.updateRoute(currentSol.pred)
    visu2.updateDist()

    relaxVariables(clusterRelax(p))
  }

  def handleObjectives() = objRelax(nextObj(), nextFloat < 0.2)

  def nextObj(): Int = {

    nObjRestart += 1

    val obj = (cp.objective.currentObjectiveIdx + 1) % nObjs

    if (nObjRestart > nObjs) {
      pareto.nextPoint
      nObjRestart = 0
    }

    cp.objective.currentObjective = obj
    obj
  }

  def objRelax(obj: Int, intensification: Boolean = false) {

    for (o <- Objs) {
      if (intensification || o == obj) {
        cp.objective.bounds(o) = pareto.currentPoint(o) - 1
      } else {
        // The -1 avoid to find an already found solution
        cp.objective.bounds(o) = pareto.currentPoint.upperValue(o) - 1
      }
    }
  }

  def clusterRelax(p: Int): Array[Boolean] = {
    if (cp.objective.currentObjectiveIdx == 0) clusterRelax1(p)
    else clusterRelax2(p)
  }

  def clusterRelax1(p: Int): Array[Boolean] = {
    
    val c = nextInt(nCities)
    val sortedByDist = Cities.sortBy(i => distMatrix1(c)(i))
    val dist = distMatrix1(c)(sortedByDist(p))

    Array.tabulate(nCities)(i => distMatrix1(c)(i) <= dist)
  }

  def clusterRelax2(p: Int): Array[Boolean] = {

    val c = nextInt(nCities)
    val sortedByDist = Cities.sortBy(i => distMatrix2(c)(i))
    val dist = distMatrix2(c)(sortedByDist(p))

    Array.tabulate(nCities)(i => distMatrix2(c)(i) <= dist)
  }

  def solFound() {
     
    newSol = true
    currentSol = new Sol(pred.map(_.value), succ.map(_.value), totDist1.value, totDist2.value)
    
    visu1.updateRoute(currentSol.pred)
    visu1.updateDist()

    visu2.updateRoute(currentSol.pred)
    visu2.updateDist()
  }

  def relaxVariables(selected: Array[Boolean]) {

    visu1.updateSelected(selected)
    visu1.updateRestart(nRestart)

    visu2.updateSelected(selected)
    visu2.updateRestart(nRestart)

    val constraints: Queue[Constraint] = Queue()

    for (c <- Cities) {
      if (!selected(c)) {

        if (!selected(currentSol.pred(c)))
          constraints enqueue (pred(c) == currentSol.pred(c))

        if (!selected(currentSol.succ(c)))
          constraints enqueue (succ(c) == currentSol.succ(c))
      }
    }

    cp.post(constraints.toArray)
  }

  // Constraints
  // -----------
  cp.minimize(totDist1, totDist2) subjectTo {

    // Channeling between predecessors and successors
    cp.add(new ChannelingPredSucc(cp, pred, succ))

    // Consistency of the circuit with Strong filtering
    cp.add(circuit(succ), Strong)
    cp.add(circuit(pred), Strong)

    // Total distance 1
    cp.add(sum(Cities)(i => distMatrix1(i)(succ(i))) == totDist1)
    cp.add(sum(Cities)(i => distMatrix1(i)(pred(i))) == totDist1)

    cp.add(new TONOTCOMMIT(cp, pred, distMatrix1, totDist1))
    cp.add(new TONOTCOMMIT(cp, succ, distMatrix1, totDist1))

    // Total distance 2
    cp.add(sum(Cities)(i => distMatrix2(i)(succ(i))) == totDist2)
    cp.add(sum(Cities)(i => distMatrix2(i)(pred(i))) == totDist2)

    cp.add(new TONOTCOMMIT(cp, pred, distMatrix2, totDist2))
    cp.add(new TONOTCOMMIT(cp, succ, distMatrix2, totDist2))
  }

  // Search
  // ------
  println("Searching...")
  cp.exploration {

    // Greedy heuristic
    if (cp.objective.currentObjectiveIdx == 0) {
      while (!allBounds(succ)) {

        val i = selectMin(Cities)(!succ(_).isBound)(succ(_).size).get
        val j = selectMin(Cities)(succ(i).hasValue(_))(distMatrix1(i)(_)).get

        cp.branch(cp.post(succ(i) == j))(cp.post(succ(i) != j))
      }
    } 
    else {
      while (!allBounds(succ)) {

        val i = selectMin(Cities)(!succ(_).isBound)(succ(_).size).get
        val j = selectMin(Cities)(succ(i).hasValue(_))(distMatrix2(i)(_)).get

        cp.branch(cp.post(succ(i) == j))(cp.post(succ(i) != j))
      }
    }

    solFound()
  }

  cp.printStats()
}
