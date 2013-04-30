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

package oscar.visual

import java.awt.geom.Line2D
import java.awt.Color

import oscar.algo.CumulativeProfile
import oscar.cp.scheduling._
import oscar.cp.core.CPVarInt

/**
 * @author Pierre Schaus, pschaus@gmail.com
 */
class VisualGrid(nRow: Int, nCol: Int) extends VisualDrawing(false) {

  val scale = 100
  val board = Array.tabulate(nRow, nCol) { case (i, j) => new VisualRectangle(this, 50 + j * scale, 50 + i * scale, scale, scale) }
  
  def apply(i: Int) = board(i)

}
