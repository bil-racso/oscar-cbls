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

package oscar.demo.tsp

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.visual._
import oscar.util._
import scala.io.Source
import java.awt.Color

/**
 * tsp model with visualization:
 * given a distance matrix between 20 cities,
 * find the shortest tour visiting each city exactly once.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object TSPDemo extends App {

  // Visualization
  // -------------
  val f = new VisualFrame("TSP")
  // creates the plot and place it into the frame
  val plot = new Plot2D("", "Solution number", "Distance")
  f.createFrame("TSP Objective Function").add(plot)
  // creates the tour visu and place it into the frame
  val map = new VisualMap();
  f.createFrame("TSP Tour").add(map)
  f.pack()

  // Data
  // ----
  val countries = Array(
    ("Albania", 41.153332, 20.168331),
    ("Andorra", 42.546245, 1.601554),
    ("Austria", 47.516231, 14.550072),
    ("Austria", 47.516231, 14.550072),
    ("Belgium", 50.503887, 4.469936),
    ("Bulgaria", 42.733883, 25.48583),
    ("Croatia", 45.1, 15.2),
    ("Czech Republic", 49.817492, 15.472962),
    ("Denmark", 56.26392, 9.501785),
    ("Estonia", 58.595272, 25.013607),
    ("Finland", 61.92411, 25.748151),
    ("France", 46.227638, 2.213749),
    ("Germany", 51.165691, 10.451526),
    ("Greece", 39.074208, 21.824312),
    ("Ireland", 53.41291, -8.24389),
    ("Latvia", 56.879635, 24.603189),
    ("Luxembourg", 49.815273, 6.129583),
    ("Portugal", 39.399872, -8.224454),
    ("Spain", 40.463667, -3.74922),
    ("United Kingdom", 55.378051, -3.435973)
  ).map(t => new Location(t._2, t._3));

  val n = countries.size
  val Cities = 0 until n

  val rand = new scala.util.Random(0)
  
  // Model
  // -----  
  val coord = Array.tabulate(n)(i => (100 + rand.nextInt(400), rand.nextInt(400)))
  val lines = Array.tabulate(n)(i => map.createLine((countries(i).lat, countries(i).lon), (0, 0)))
  val distMatrix = Array.tabulate(n, n)((i, j) => (countries(i).distance(countries(j))).toInt)

  val cp = new CPSolver()
  //array of successors
  val succ = Array.tabulate(n)(_ => CPVarInt(cp, 0 until n))
  //total distance
  val dist = CPVarInt(cp, 0 to distMatrix.flatten.sum)

  // Visualization update
  // --------------------
  var nbSol = 0
  countries.foreach(c => map.createWaypoint(c.lat, c.lon))
  def updateVisu() {
    def update(i: Int) = lines(i).dest = (countries((succ(i).value)).lat, countries((succ(i).value)).lon)
    nbSol += 1
    (0 until n).foreach(update(_))
    plot.addPoint(nbSol, dist.value)
  }

  cp.minimize(dist) subjectTo {
    cp.add(circuit(succ), Strong) //ask to have a strong filtering
    cp.add(sum(Cities)(i => distMatrix(i)(succ(i))) == dist)
  } exploration {
    //exploration of the search tree
    while (!allBounds(succ)) {
      val (x, i) = selectMin(succ.zipWithIndex)(!_._1.isBound)(_._1.size).get
      // get the closest successor in the domain of x
      val v = argMin((x.min to x.max).filter(x.hasValue(_)))(distMatrix(i)(_)).head
      cp.branch(cp.post(x == v))(cp.post(x != v))
    }
    updateVisu()
  } run()
  
  cp.printStats()
}
