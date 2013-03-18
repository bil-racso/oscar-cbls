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
import javax.swing.SwingUtilities
import oscar.cp.mem.pareto.ListPareto
import oscar.cp.mem.pareto.MOSol


/**
 * @author Pierre Schaus
 */
 class PlotPareto(title: String = "Pareto", xlab: String = "Obj1", ylab: String = "Obj2", nbPareto: Int = 1) extends PlotScatter(title,xlab,ylab,nbPareto) {

  // TODO: Will return a null pointer exception when calling insert
  val paretos = Array.fill(nbPareto)(new ListPareto[Object](Array(null, null)))
  
  def insert(obj1: Int, obj2: Int, paretoIdx: Int = 0) {
    paretos(paretoIdx).insert(new MOSol(null, Array(obj1, obj2)))
    SwingUtilities.invokeLater(new Runnable() {
      def run() {
        removeAllPoints(paretoIdx)
        for (p <- paretos(paretoIdx)) {
          val x = p(0)
          val y = p(1)
          addPoint(x, y, paretoIdx)
        }
      }
    })
  }

  
}