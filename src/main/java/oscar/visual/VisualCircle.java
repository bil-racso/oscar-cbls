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
