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
public class VisualLine extends ColoredShape<Line2D.Double>{
	
	public Line2D.Double line;
	
	
	public VisualLine(VisualDrawing d,double xorig, double yorig, double xdest, double ydest) {
		super(d, new Line2D.Double(xorig,yorig,xdest,ydest));
		line = shape;
	}
	
	/**
	 * Move the destination point
	 * @param x
	 * @param y
	 */
	public void setDest(double x, double y) {
		line.setLine(line.getX1(),line.getY1(),x,y);
		drawing.repaint();
	}
	
	/**
	 * Move the origin point
	 * @param x
	 * @param y
	 */
	public void setOrig(double x, double y) {
		line.setLine(x,y,line.getX2(),line.getY2());
		drawing.repaint();
	}
	
	
	
}
