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

import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;

import javax.swing.JInternalFrame;

import scala.Tuple2;


/**
 * 
 * @author Pierre Schaus
 *
 */
public class VisualText extends ColoredShape<Rectangle2D> {
	
	private String text;
	private int x,y;
	private boolean centered = false;
	
	public void setText(String str) {
		text = str;
	}
	
	public void setCentered(boolean b) {
		centered = b;
	}
	
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
		drawing().repaint();
	}
	
	@Override
	public void draw(Graphics2D g) {
		
		g.setColor(innerCol());
		
		if (centered) 
			drawCenteredString(text, x, y, g);
		else
			g.drawString(text, x, y);
	}
	
	public void drawCenteredString(String text, int x, int y, Graphics2D g) {
	
	    FontMetrics fm = g.getFontMetrics();
	    
	    int w = fm.stringWidth(text);
	    g.drawString(text, x-(w/2), y);
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
			
			arrow.dest_$eq(new Tuple2(100.0, 100.0));
			text.move(100, 100);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

	}
	
		
	

	
	
	
}
