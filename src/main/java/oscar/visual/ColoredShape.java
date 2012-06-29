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
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.Point2D;

public class ColoredShape<E extends Shape> {


	private Color innerCol = Color.white;
	private Color outerCol = Color.black;
	
	protected VisualDrawing drawing;
	public E shape;
	private boolean visible = true;
	
	public String toolTip = null;
	
	
	public ColoredShape(VisualDrawing d, E shape) {
		this.drawing = d;
		this.shape = shape;
		d.addShape(this);
	}	
	
	public void draw(Graphics2D g) {
		if (visible) {
			g.setColor(getInnerCol());
			g.fill(shape);
			g.setColor(getOuterCol());
			g.draw(shape);
		}
	}
	
	public void setVisible(boolean visible) {
		this.visible = visible;
		drawing.repaint();
	}

	public Color getInnerCol() {
		return innerCol;
	}

	public void setInnerCol(Color innerCol) {
		this.innerCol = innerCol;
		drawing.repaint();
	}

	public Color getOuterCol() {
		return outerCol;
	}

	public void setOuterCol(Color outerCol) {
		this.outerCol = outerCol;
		drawing.repaint();
	}
	
	public void setToolTip(String text) {
		this.toolTip = text;
	}
	
	protected void showToolTip(Point2D mousePoint) {
		if (toolTip != null && shape.contains(mousePoint)) {
			drawing.showToolTip(toolTip);
		}
	}
	
	
	
	
}
