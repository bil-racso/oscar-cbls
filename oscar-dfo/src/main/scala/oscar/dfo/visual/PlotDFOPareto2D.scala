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

package oscar.dfo.visual

import oscar.visual._
import javax.swing.SwingUtilities
import oscar.dfo.utils._
import oscar.visual.plot.PlotScatter

/**
 * @author Cyrille Dejemeppe
 * @author Pierre Schaus
 */
class PlotDFOPareto2D[E <% Ordered[E]] (
  title: String,
  xlab: String, 
  ylab: String, 
  nbPareto: Int, 
  objMax1: Boolean, 
  objMax2: Boolean
) extends PlotScatter(title, xlab, ylab, nbPareto) {
  
  var currentIterate: MOOPoint[E] = null

  def highLightIterate(iterate: ArchiveElement[E]) {
    currentIterate = iterate.getMOOPoint
    highlight(iterate.getMOOPoint.getEvaluation(0).asInstanceOf[Double], iterate.getMOOPoint.getEvaluation(1).asInstanceOf[Double])
  }

  def update(archive: Set[ArchiveElement[E]], paretoIndex: Int = 0) {
    SwingUtilities.invokeLater(new Runnable() {
      def run() {
        removeAllPoints(paretoIndex)
        if (currentIterate != null) {
          addPoint(currentIterate.getEvaluation(0).asInstanceOf[Double], currentIterate.getEvaluation(1).asInstanceOf[Double], paretoIndex)
          highLightIterate(currentIterate)
        }
        for (p <- archive) {
          val x = p.getEvaluation(0).asInstanceOf[Double]
          val y = p.getEvaluation(1).asInstanceOf[Double]
          addPoint(x, y, paretoIndex)
        }
      }
    })
  }
}

object PlotDFOPareto2D {
  def apply[E <% Ordered[E]] (
    title: String = "Pareto",
    xlab: String = "Obj1",
    ylab: String = "Obj2",
    nbPareto: Int = 1,
    objMax1: Boolean = false, // true if obj1 is a maximization objective
    objMax2: Boolean = false // true if obj2 is a maximization objective
  ) = new PlotDFOPareto2D[E](title, xlab, ylab, nbPareto, objMax1, objMax2)
}
