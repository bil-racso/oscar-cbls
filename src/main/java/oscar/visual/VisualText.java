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

import java.awt.Graphics2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;

import javax.swing.JInternalFrame;


/**
 * 
 * @author Pierre Schaus
 *
 */
public class VisualText extends ColoredShape<Rectangle2D> {
	
	private String text;
	private int x,y;
	
	public VisualText(VisualDrawing d,int x, int y, String text) {
		super(d, null);
		this.text = text;
		this.x = x;
		this.y = y;
	}
	
	/**
	 * Move the specified left corner
	 * @param x
	 * @param y
	 */
	public void move(int x, int y) {
		this.x = x;
		this.y = y;
		drawing.repaint();
	}
	
	@Override
	public void draw(Graphics2D g) {
		g.drawString(text, x, y);
	}
	

	public static void main(String[] args) {


		try {
			VisualFrame f = new VisualFrame("toto");
			VisualDrawing d = new VisualDrawing(false);
			JInternalFrame inf = f.createFrame("Drawing");
			inf.add(d);
			f.pack();

			VisualArrow arrow = new VisualArrow(d, 50, 50, 100, 50,5);
			VisualText text = new VisualText(d, 50, 50, "hello");
			
			Thread.sleep(1000);
			
			arrow.setDest(100, 100);
			text.move(100, 100);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

	}
	
		
	

	
	
	
}
