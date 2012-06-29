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
 ******************************************************************************/
package oscar.visual;

import java.awt.Color;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;


import javax.swing.JInternalFrame;

/**
 * 
 * @author Pierre Schaus
 *
 */
public class VisualCircle extends ColoredShape<Ellipse2D.Double>{

	Ellipse2D.Double ellipse;
	private double radius;
	private double centerx;
	private double centery;

	public VisualCircle(VisualDrawing d,double x, double y, double radius) {
		super(d, new Ellipse2D.Double(x-radius,y-radius,2*radius,2*radius));
		ellipse = shape;
		this.radius = radius;
		this.centerx = x;
		this.centery = y;
	}
	
	public VisualCircle(VisualDrawing d,double x, double y, double radius, Color col) {
		this(d,x,y,radius);
		setInnerCol(col);
	}
	
	/**
	 * X coordinates of the center
	 * @return
	 */
	public double getX() {
		return centerx;
	}

	/**
	 * Y coordinates of the center
	 * @return
	 */
	public double getY() {
		return centery;
	}
	
	/**
	 * radius of circle
	 * @return
	 */
	public double getRadius() {
		return radius;
	}
	
	private void update() {
		ellipse.setFrame(centerx-radius,centery-radius,2*radius,2*radius);
		drawing.repaint();
	}

	
	/**
	 * Move the center to x,y
	 * @param x
	 * @param y
	 */	
	public void move(double x, double y) {
		centerx = x;
		centery = y;
		update();
	}
	
	public void setRadius(double radius) {
		this.radius = radius;
		update();
	}	




	public static void main(String[] args) {


		try {
			VisualFrame f = new VisualFrame("toto");
			VisualDrawing d = new VisualDrawing(false);
			JInternalFrame inf = f.createFrame("Drawing");
			inf.add(d);
			f.pack();

			VisualCircle circ = new VisualCircle(d, 100, 100, 50);
			Thread.sleep(1000);
			circ.setInnerCol(Color.blue);
			Thread.sleep(1000);
			circ.setRadius(100);
			Thread.sleep(1000);
			
			circ.move(300, 200);
			for (int i = 0; i < 20; i++) {
				Thread.sleep(50);
				circ.move(circ.getX()+5, circ.getY());
			}
			
			
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

	}

}
