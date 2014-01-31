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
import oscar.dfo.multiobjective.mogen.MOGEN
import oscar.dfo.utils.MOEvaluator
import oscar.visual.VisualFrame
import oscar.dfo.visual.PlotDFOPareto2D
import oscar.dfo.multiobjective.mogen.algos.NelderMead

/**
 * @author cyrille.dejemeppe@gmail.com
 */
object ZDT1WithMOGENNM extends App {
  
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
  val mogenSolver = MOGEN(evaluator)

  mogenSolver.initFeasibleReagion(List(inUnitHV))
  mogenSolver.initSinglePointArchive(Array.fill(nbCoordinates)((0.0, 1.0)), List((NelderMead, 1.0)))//initRandomArchive(100, Array.fill(nbCoordinates)((0.0, 1.0)), List((NelderMead, 1.0)))
  
  val f = VisualFrame("toto")
  val inf = f.createFrame("Drawing")
  val paretoPlot = PlotDFOPareto2D[Double]()
  inf.add(paretoPlot)
  inf.pack()
  paretoPlot.xDom = 0 to 1
  paretoPlot.yDom = 0 to 1
  
  
  for (i <- 1 to 10000) {
    mogenSolver.performIteration(i)
    paretoPlot.update(mogenSolver.archive.toSet, 0)
  }
  println("Nb points in the Pareto front estimation: " + mogenSolver.archive.size)
  println("Nb calls to evaluation functions: " + mogenSolver.evaluator.nbCallToEvalFunction)

}
