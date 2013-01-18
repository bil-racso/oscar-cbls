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
package oscar.visual;

import java.awt.geom.Line2D;


/**
 * 
 * @author Pierre Schaus
 *
 */
class VisualLine(d:VisualDrawing, shape:Line2D.Double) extends ColoredShape[Line2D.Double](d,shape){
	
	
	
	def line:Line2D.Double = shape
	
	def this(d:VisualDrawing,xorig:Double, yorig:Double, xdest:Double, ydest:Double) {
		this(d, new Line2D.Double(xorig,yorig,xdest,ydest))
		
	}
	
	/**
	 * Move the destination point
	 * @param x
	 * @param y
	 */
	def setDest(x:Double, y:Double) : Unit = {
		line.setLine(line.getX1(),line.getY1(),x,y)
		drawing.repaint()
	}
	
	/**
	 * Move the origin point
	 * @param x
	 * @param y
	 */
	def setOrig(x:Double, y:Double) : Unit = {
		line.setLine(x,y,line.getX2(),line.getY2())
		drawing.repaint()
	}
	
	
	
}
