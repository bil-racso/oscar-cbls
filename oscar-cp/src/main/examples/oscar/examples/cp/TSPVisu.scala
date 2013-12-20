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

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.algo.search._
import oscar.cp.core._
import oscar.util._
import oscar.visual._
import scala.collection.JavaConversions._
import scala.io.Source
import java.lang._
import java.awt.Color
import oscar.cp.constraints.MinAssignment
import oscar.visual.shapes.VisualLine
import oscar.visual.shapes.VisualCircle
import oscar.visual.plot.PlotLine

/**
 * Traveling Salesman Problem with Visualization
 *
 * Given a distance matrix between 20 cities,
 * find the shortest tour visiting each city exactly once.
 *
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
object TSPVisu extends App {

  val nCities = 20
  val Cities = 0 until nCities

  // Data parsing
  // ------------
  val rand = new scala.util.Random(0)

  // Random coordinates
  val coord = Array.tabulate(nCities)(i => (100 + rand.nextInt(400), rand.nextInt(400)))

  // Computes the distance between two cities
  def getDist(p1: (Int, Int), p2: (Int, Int)): Int = {
    val dx = p2._1 - p1._1
    val dy = p2._2 - p1._2
    math.sqrt(dx * dx + dy * dy).toInt
  }

  // Builds the distance matrix
  val distMatrix = Array.tabulate(nCities, nCities)((i, j) => getDist(coord(i), coord(j)))

  // Model
  // -----
  val cp = new CPSolver()

  // Successors
  val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
  // Total distance
  val totDist = CPVarInt(cp, 0 to distMatrix.flatten.sum)

  // Visual Components
  // -----------------
  val f = VisualFrame("TSP")

  // Creates the plot and place it into the frame
  val plot = new PlotLine("", "Solution number", "Distance")
  f.createFrame("TSP Objective Function").add(plot)

  // Creates the visualization of the tour and place it into the frame
  val tour = VisualTour(coord)
  f.createFrame("TSP Tour").add(tour)
  f.pack()
  
  // Updates the visualization
  def updateVisu() {
    Cities.foreach(i => tour.edgeDest(i, succ(i).value))
    tour.repaint()
    plot.addPoint(nbSol, totDist.value)
  }

  // Constraints + Search
  // --------------------
  var nbSol = 0
  cp.minimize(totDist) subjectTo {
    // Consistency of the circuit with Strong filtering
    cp.add(circuit(succ), Strong)
    cp.add(new MinAssignment(succ,distMatrix,totDist))
    // Total distance
    cp.add(sum(Cities)(i => distMatrix(i)(succ(i))) == totDist)

  } search {
    binaryFirstFail(succ)
  } onSolution {
    // Updates the visualization
    nbSol += 1
    updateVisu()
  }
  
  println(cp.start())
}
