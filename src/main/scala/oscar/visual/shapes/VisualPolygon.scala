/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.visual.shapes

import java.awt.Polygon
import oscar.visual.VisualDrawing

class VisualPolygon(d: VisualDrawing) extends VisualShape(d, new Polygon) {

  def reset(): Unit = shape.reset

  def addVertices(vertices: Iterable[(Int, Int)]): Unit = {
    for (v <- vertices) shape.addPoint(v._1, v._2)
  }

  def update(vertices: Iterable[(Int, Int)]): Unit = {
    reset()
    addVertices(vertices)
  }

  def move(x: Double, y: Double): Unit = shape.translate(x.toInt, y.toInt)
}
