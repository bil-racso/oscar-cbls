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
package oscar.visual;

import java.awt.geom.Line2D;


/**
 * 
 * @author Pierre Schaus
 *
 */
class VisualLine(d:VisualDrawing, shape:Line2D.Double) extends ColoredShape[Line2D.Double](d,shape){
	
	def line:Line2D.Double = shape
	
	def this(d:VisualDrawing,xorig:Double = 0, yorig:Double = 0, xdest:Double = 0, ydest:Double =0) {
		this(d, new Line2D.Double(xorig,yorig,xdest,ydest))
		
	}
	
	/**
	 * Move the destination point
	 * @param x
	 * @param y
	 */
	def dest_=(d:(Double,Double)) : Unit = {
		line.setLine(line.getX1(),line.getY1(),d._1,d._2)
		drawing.repaint()
	}
	
	/**
	 * Move the origin point
	 * @param x
	 * @param y
	 */
	def orig_=(d:(Double,Double)) : Unit = {
		line.setLine(d._1,d._2,line.getX2(),line.getY2())
		drawing.repaint()
	}
	
	def orig = (line.getX2(),line.getY2())
	def dest = (line.getX1(),line.getY1())
	
}
