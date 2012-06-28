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
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;

import javax.swing.JInternalFrame;


/**
 * 
 * @author Pierre Schaus
 *
 */
public class VisualArrow extends VisualLine{
	
	
	
	Polygon arrowHead = new Polygon(); 
	
	public VisualArrow(VisualDrawing d,double xorig, double yorig, double xdest, double ydest,int dim) {
		super(d,xorig,yorig,xdest,ydest);
		
		 
		arrowHead.addPoint( 0,dim);
		arrowHead.addPoint( -dim, -dim);
		arrowHead.addPoint( dim,-dim);

	}
	
    @Override
    public void draw(Graphics2D g) {
    	super.draw(g);
    	drawArrowHead(g);
    }
    
    private void drawArrowHead(Graphics2D g2d) { 
    	
    	AffineTransform tx = new AffineTransform();
        tx.setToIdentity();
        double angle = Math.atan2(line.y2-line.y1, line.x2-line.x1);
        tx.translate(line.x2, line.y2);
        tx.rotate((angle-Math.PI/2d));  
        g2d.fill(tx.createTransformedShape(arrowHead));
    }
    

	public static void main(String[] args) {


		try {
			VisualFrame f = new VisualFrame("toto");
			VisualDrawing d = new VisualDrawing(false);
			JInternalFrame inf = f.createFrame("Drawing");
			inf.add(d);
			f.pack();

			VisualArrow arrow = new VisualArrow(d, 50, 50, 100, 50,5);
			
			Thread.sleep(1000);
			
			arrow.setDest(100, 100);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

	}
	
	
}
