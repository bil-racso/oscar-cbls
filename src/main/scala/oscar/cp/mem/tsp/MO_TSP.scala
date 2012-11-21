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
import oscar.cp.mem.RoutingUtils._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._
import oscar.cp.constraints._
import oscar.cp.mem.visu.VisualRelax
import scala.collection.mutable.Queue
import scala.util.Random.nextInt
import scala.util.Random.nextFloat
import scala.math.round
import oscar.search.IDSSearchController
import oscar.cp.mem.pareto.ParetoMinSet
import java.io._
import oscar.cp.mem.ChannelingPredSucc

/*val bestPred0 = Array(46, 43, 42, 96, 51, 62, 8, 91, 56, 83, 14, 26, 75, 2, 16, 93, 58, 78, 89, 11, 71, 69, 97, 17, 80, 64, 85, 66, 33, 47, 88, 10, 36, 82, 61, 98, 4, 23, 29, 53, 70, 7, 45, 49, 31, 28, 92, 99, 5, 72, 86, 77, 87, 1, 6, 79, 19, 60, 73, 76, 50, 59, 0, 39, 3, 25, 57, 84, 63, 65, 13, 9, 67, 20, 18, 32, 22, 95, 52, 30, 68, 94, 54, 35, 81, 34, 24, 15, 41, 48, 44, 74, 27, 21, 12, 38, 55, 90, 37, 40)
  val bestSucc0 = Array(62, 53, 13, 64, 36, 48, 54, 41, 6, 71, 31, 19, 94, 70, 10, 87, 14, 23, 74, 56, 73, 93, 76, 37, 86, 65, 11, 92, 45, 38, 79, 44, 75, 28, 85, 83, 32, 98, 95, 63, 99, 88, 2, 1, 90, 42, 0, 29, 89, 43, 60, 4, 78, 39, 82, 96, 8, 66, 16, 61, 57, 34, 5, 68, 25, 69, 27, 72, 80, 21, 40, 20, 49, 58, 91, 12, 59, 51, 17, 55, 24, 84, 33, 9, 67, 26, 50, 52, 30, 18, 97, 7, 46, 15, 81, 77, 3, 22, 35, 47)
  val bestDist01 = 21282
  val bestDist02 = 178446
  
  val bestPred1 = Array(94, 15, 10, 82, 61, 3, 83, 98, 33, 20, 92, 0, 62, 41, 5, 49, 77, 44, 43, 79, 89, 54, 21, 17, 8, 99, 70, 2, 7, 48, 47, 58, 14, 6, 60, 95, 71, 19, 39, 66, 16, 1, 88, 40, 35, 24, 64, 50, 85, 42, 81, 53, 69, 87, 76, 80, 93, 51, 75, 56, 26, 68, 30, 13, 36, 73, 4, 9, 25, 38, 11, 37, 52, 59, 29, 28, 23, 12, 46, 74, 78, 32, 63, 57, 72, 67, 65, 22, 86, 45, 27, 18, 84, 34, 97, 91, 90, 31, 96, 55)
  val bestSucc1 = Array(11, 41, 27, 5, 66, 14, 33, 28, 24, 67, 2, 70, 77, 63, 32, 1, 40, 23, 91, 37, 9, 22, 87, 76, 45, 68, 60, 90, 75, 74, 62, 97, 81, 8, 93, 44, 64, 71, 69, 38, 43, 13, 49, 18, 17, 89, 78, 30, 29, 15, 47, 57, 72, 51, 21, 99, 59, 83, 31, 73, 34, 4, 12, 82, 46, 86, 39, 85, 61, 52, 26, 36, 84, 65, 79, 58, 54, 16, 80, 19, 55, 50, 3, 6, 92, 48, 88, 53, 42, 20, 96, 95, 10, 56, 0, 35, 98, 94, 7, 25)
  val bestDist11 = 176436
  val bestDist12 = 22141*/

object MO_TSP {

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  def main(args: Array[String]) {

    for (i <- 1 to 1) {
      val results = solveTSP(2000, 1000)
      val rPrint = results.map(e => e._1 + " " + e._2)
      printToFile(new File("twoPhase" + i + ".txt"))(p => {
        rPrint.foreach(p.println)
      })
    }
  }

  def solveTSP(maxRestart: Int, maxFail: Int): Array[(Int, Int)] = {

    case class Sol(pred: Array[Int], succ: Array[Int], dist1: Int, dist2: Int)

    val nObjs = 2
    val Objs = 0 until nObjs
    val pareto = ParetoMinSet[Sol]()

    new TspModel("data/TSP/kroA100.tsp", "data/TSP/kroB100.tsp") {

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
      val visu1: VisualRelax = new VisualRelax(coord1, realDistMatrix1)
      val visu2: VisualRelax = new VisualRelax(coord2, realDistMatrix2)

      // MOLNS
      // -----
      var newSol: Sol = null

      var nRestart = 1
      var nObjRestart = 0
      var nStagnation = 0

      val pMin = 15
      val pMax = 60
      var p = pMin

      var firstLns = true

      cp.lns(maxRestart, maxFail) {

        // First LNS
        if (firstLns) {
          println("Start LNS");
          firstLns = false
        }

        nRestart += 1

        // Adds new solutions    
        if (newSol != null) {
          if (pareto insert ((newSol.dist1, newSol.dist2), newSol)) nObjRestart = 0         
          newSol = null
          nStagnation = 0
          p = pMin + (p - pMin) / 2
        } else {
          nStagnation += 1
          if (nStagnation == 20) {
        	  nStagnation = 0
        	  if (p < pMax) p += 2
          }
        }

        println("PARETO SIZE " + pareto.size)

        if (nRestart < 200) improveObj(0)
        else if (nRestart < 400) improveObj(1)
        else diversif()

        visu1.updateRoute(pareto.currentSol.pred)
        visu1.updateDist()
        visu2.updateRoute(pareto.currentSol.pred)
        visu2.updateDist()

        relaxVariables(pathRelax(p))
      }

      def nextObj(): Int = {

        nObjRestart += 1

        val obj = (cp.objective.currentObjectiveIdx + 1) % nObjs

        if (nObjRestart > nObjs) {
          pareto.nextSol(0)
          nObjRestart = 1
        }

        cp.objective.currentObjective = obj
        obj
      }

      def objRelax(obj: Int, intensification: Boolean) {
        for (o <- Objs) {
          if (intensification || o == obj) cp.objective.bounds(o) = pareto.currentVal(o) - 1
          else cp.objective.bounds(o) = pareto.currentUB(o) - 1
        }
      }

      def improveObj(obj: Int) {
        pareto.bestSol(obj)
        nObjRestart = 0
        cp.objective.currentObjective = obj
        objRelax(obj, false)
      }

      def diversif() {
        pareto.bestDivSurf
        val obj = nextObj()
        objRelax(obj, nextFloat < 0.3)
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

      def pathRelax(p: Int): Array[Boolean] = {

        val c = nextInt(nCities)
        val selected = Array.fill(nCities)(false)
        selected(c) = true

        for (i <- 1 until p) {

          val sel = Cities.filter(i => selected(i))
          val rem = Cities.filter(i => !selected(i))

          val c = sel(nextInt(sel.size))
          val cc = if (cp.objective.currentObjectiveIdx == 0) rem.sortBy(i => distMatrix1(c)(i)).head
          else rem.sortBy(i => distMatrix2(c)(i)).head

          selected(cc) = true
        }
        selected
      }

      def clusterRelax2(p: Int): Array[Boolean] = {

        val c = nextInt(nCities)
        val sortedByDist = Cities.sortBy(i => distMatrix2(c)(i))
        val dist = distMatrix2(c)(sortedByDist(p))

        Array.tabulate(nCities)(i => distMatrix2(c)(i) <= dist)
      }

      def solFound() {

        newSol = new Sol(pred.map(_.value), succ.map(_.value), totDist1.value, totDist2.value)

        visu1.updateRoute(newSol.pred)
        visu1.updateDist()

        visu2.updateRoute(newSol.pred)
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

            if (!selected(pareto.currentSol.pred(c)))
              constraints enqueue (pred(c) == pareto.currentSol.pred(c))

            if (!selected(pareto.currentSol.succ(c)))
              constraints enqueue (succ(c) == pareto.currentSol.succ(c))
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
        if (cp.objective.currentObjectiveIdx == 0) regretHeuristic(cp, succ, distMatrix1)
        else regretHeuristic(cp, succ, distMatrix2)

        solFound()
      }

    }

    return pareto.points.map(p => (p.obj1, p.obj2))
  }
}
