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
package oscar.examples.dfo

import oscar.dfo._
import oscar.algebra._
import oscar.dfo.multiobjective.evolutionary.SPEA2
import oscar.dfo.utils.MOEvaluator
import oscar.dfo.utils.MinMOOComparator
import oscar.visual.VisualFrame
import oscar.visual.plot.PlotScatter
import java.awt.Color
import oscar.dfo.multiobjective.evolutionary.EvolutionaryElement
import oscar.dfo.visual.PlotDFOPareto2D

/**
 * @author cyrille.dejemeppe@gmail.com
 */
object ZDT1 extends App {
  
  // The zdt1 objective
  def zdt1(coordinates: Array[Double]): Array[Double] = {
    val f1 = coordinates(0)
    val g = 1.0 + (9 / (coordinates.length - 1)) * coordinates.drop(1).sum
    val f2 = g * (1.0 - math.sqrt(f1 / g))
    Array(f1, f2)
  }
  
  def inUnitHV(coordinates: Array[Double]): Boolean = {
    for (coord <- coordinates) {
      if (coord < 0 || coord > 1) return false
    }
    true
  }
  
  val nbCoordinates = 2

  // declare two variables and their domain
  val evaluator = MOEvaluator(zdt1, Array.fill(2)(Double.MaxValue))
  val comparator = MinMOOComparator[Double]()
  val populationSize = 100
  val archiveSize = 100
  val mutationProba = 100
  val spea2Solver = SPEA2(evaluator, comparator,
		  				  populationSize, archiveSize, mutationProba)

  spea2Solver.initPopulation(Array.fill(nbCoordinates)((0.0, 1.0)))
  spea2Solver.initFeasibleReagion(List(inUnitHV))
  
  val f = VisualFrame("toto")
  val inf = f.createFrame("Drawing")
  val paretoPlot = PlotDFOPareto2D[Double]()
  inf.add(paretoPlot)
  inf.pack()
  paretoPlot.xDom = 0 to 1
  paretoPlot.yDom = 0 to 1
  
  
  for (i <- 1 to 1000) {
    spea2Solver.performIteration(i)
    paretoPlot.update(spea2Solver.archive.toSet, 0)
  }

}
