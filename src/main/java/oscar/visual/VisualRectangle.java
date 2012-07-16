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

import java.awt.Color;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;

import javax.swing.JInternalFrame;

/**
 * 
 * @author Pierre Schaus
 *
 */
public class VisualRectangle extends ColoredShape<Rectangle2D.Double>{

	Rectangle2D.Double rect;

	public VisualRectangle(VisualDrawing d,double x, double y, double w, double h) {
		super(d, new Rectangle2D.Double(x,y,w,h));
		rect = shape;
	}
	
	/**
	 * X coordinates of bottom left corner
	 * @return
	 */
	public double getX() {
		return rect.getX();
	}

	/**
	 * Y coordinates of bottom left corner
	 * @return
	 */
	public double getY() {
		return rect.getY();
	}
	
	/**
	 * width of the rectangle
	 * @return
	 */
	public double getWidth() {
		return rect.getWidth();
	}
	
	/**
	 * height of the rectangle
	 * @return
	 */
	public double getHeight() {
		return rect.getHeight();
	}
	
	/**
	 * Move the specified left corner
	 * @param x
	 * @param y
	 */
	public void move(double x, double y) {
		rect.setRect(x, y, rect.getWidth(), rect.getHeight());
		drawing.repaint();
	}

	/**
	 * Set width
	 * @param w
	 */
	public void setWidth(double w) {
		rect.setRect(rect.getX(), rect.getY(), w, rect.getHeight());
		drawing.repaint();
	}


	/**
	 * Set height
	 * @param w
	 */
	public void setHeight(double h) {
		rect.setRect(rect.getX(), rect.getY(), rect.getWidth(), h);
		drawing.repaint();
	}

	public static void main(String[] args) {


		try {
			VisualFrame f = new VisualFrame("toto");
			VisualDrawing d = new VisualDrawing(false);
			JInternalFrame inf = f.createFrame("Drawing");
			inf.add(d);
			f.pack();

			VisualRectangle rect = new VisualRectangle(d, 50, 50, 100, 50);
			rect.setToolTip("Hello");
			
			
			Thread.sleep(1000);
			rect.setInnerCol(Color.red);
			Thread.sleep(1000);
			rect.setWidth(200);
			Thread.sleep(1000);
			rect.setHeight(100);
			Thread.sleep(1000);
			rect.move(100, 20);
			for (int i = 0; i < 20; i++) {
				Thread.sleep(50);
				rect.move(rect.getX()+5, rect.getY());
			}
			
			
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

}
