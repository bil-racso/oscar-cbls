/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
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
