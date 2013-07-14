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
package oscar.visual.shapes

import java.awt.Polygon
import oscar.visual.VisualDrawing

class VisualPolygon(d : VisualDrawing) extends VisualShape(d, new Polygon) {
	
	def reset = { shape.reset }
	
	def draw(points : Array[Tuple2[Int, Int]]) = {
		
		for (i <- 0 until points.size)
			shape.addPoint(points(i)._1, points(i)._2)
	}
	
	def update(points : Array[Tuple2[Int, Int]]) = {
		
		reset
		for (i <- 0 until points.size)
			shape.addPoint(points(i)._1, points(i)._2)
	}
}
