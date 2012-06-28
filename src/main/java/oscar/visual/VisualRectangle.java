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
