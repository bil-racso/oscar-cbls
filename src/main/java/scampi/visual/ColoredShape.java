/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.visual;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Shape;

public class ColoredShape<E extends Shape> {


	private Color innerCol = Color.white;
	private Color outerCol = Color.black;
	
	protected VisualDrawing drawing;
	public E shape;
	private boolean visible = true;
	
	
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
	
}
