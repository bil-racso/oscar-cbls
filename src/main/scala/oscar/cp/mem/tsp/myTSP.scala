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
import scala.collection.mutable.Queue
import scala.util.Random.nextInt
import scala.util.Random.nextFloat
import scala.math.round
import scala.math.pow
import oscar.search.IDSSearchController
import oscar.cp.mem.RoutingUtils
import oscar.cp.mem.constraints.ChannelingPredSucc
import oscar.cp.mem.constraints.InSet

object myTSP extends App {

  // Data parsing
  // ------------
  val coord = TSPUtils.parseCoordinates("data/TSP/renA10.tsp")
  val realMatrix = TSPUtils.buildRealDistMatrix(coord)
  val distMatrix = TSPUtils.buildDistMatrix(coord)

  val nCities = distMatrix.size
  val Cities  = 0 until nCities

  // Model
  // -----
  val cp = new CPSolver()

  // Successors
  val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
  // Predecessors
  val pred = Array.fill(nCities)(CPVarInt(cp, Cities))
  // Total distance
  val totDist = CPVarInt(cp, 0 to distMatrix.flatten.sum)

  // Constraints
  // -----------
  cp.minimize(totDist) subjectTo {

    cp.add(ChannelingPredSucc(pred, succ))

    cp.add(circuit(succ))
    cp.add(circuit(pred))

    cp.add(sum(Cities)(i => distMatrix(i)(succ(i))) == totDist)
    cp.add(sum(Cities)(i => distMatrix(i)(pred(i))) == totDist)
    cp.add(new MinAssignment(pred, distMatrix, totDist))
    cp.add(new MinAssignment(succ, distMatrix, totDist))
  }

  // Search
  // ------
  println("Searching...")
  cp.exploration {
    RoutingUtils.regretHeuristic(cp, pred, distMatrix)
    solFound()
  }

  // LNS
  // ---
  case class Sol(pred: Array[Int], succ: Array[Int])

  var currentSol: Sol = null
  var p = 5
  
  // Get initial solution
  cp.run(1)
  
  for (iter <- 1 to 100) {    
    cp.runSubjectTo(Int.MaxValue, 1000) {
      relaxVariables(clusterRelax(p))
    }
  }

  def clusterRelax(p: Int): Array[Boolean] = {

    val c = nextInt(nCities)
    val sortedByDist = Cities.sortBy(i => distMatrix(c)(i))
    val dist = distMatrix(c)(sortedByDist(p))

    Array.tabulate(nCities)(i => distMatrix(c)(i) <= dist)
  }

  def solFound() = {
    currentSol = new Sol(pred.map(_.value), succ.map(_.value))
  }

  def relaxVariables(selected: Array[Boolean]) {

    val constraints: Queue[Constraint] = Queue()

    for (c <- Cities; if !selected(c)) {

      val p = currentSol.pred(c)
      val s = currentSol.succ(c)

      if (!selected(p) && !selected(s)) {
        constraints.enqueue(new InSet(cp, pred(c), Set(p, s)))
        constraints.enqueue(new InSet(cp, succ(c), Set(p, s)))
      }
    }
    
    val notSelected = Cities.filter(selected(_))
    val rand = cp.random.nextInt(notSelected.size)
    val s = notSelected(rand)
    constraints.enqueue(pred(s) == (if (cp.random.nextBoolean()) currentSol.pred(s) else currentSol.succ(s)))
  
    cp.post(constraints.toArray)
  }
}
