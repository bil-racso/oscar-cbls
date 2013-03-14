/*******************************************************************************
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
 ******************************************************************************/


package oscar.cp.mem.visu

import oscar.visual._
import java.awt.Color
import java.awt.Dimension
import oscar.cp.mem.pareto.Pareto
import org.jdesktop.swingx.decorator.ComponentAdapter
import java.awt.event.ComponentEvent
import oscar.cp.mem.pareto.ParetoObserver
import oscar.cp.mem.pareto.ParetoSet
import javax.swing.SwingUtilities


/**
 * @author Pierre Schaus
 */
 class PlotPareto[Sol](val pareto1: Pareto[Sol], val pareto2: Pareto[Sol] = new ParetoSet[Sol](2), title: String = "Pareto", xlab: String = "Obj1", ylab: String = "Obj2") extends PlotScatter(title,xlab,ylab,2) with ParetoObserver {


  pareto1.addObserver(this)
  pareto2.addObserver(this)
  update()
  
 
  def update() {
    SwingUtilities.invokeLater(new Runnable() {
      def run() {
        removeAllPoints(0)
        removeAllPoints(1)

        for (p <- pareto1) {
          val x = p(0)
          val y = p(1)
          addPoint(x, y, 0)
        }

        for (p <- pareto2) {
          val x = p(0)
          val y = p(1)
          addPoint(x, y, 1)
        }
        chart.fireChartChanged()

      }
    })
  }
  

  
  
}