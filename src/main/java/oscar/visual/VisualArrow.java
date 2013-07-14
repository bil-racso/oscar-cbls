/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.visual;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.AffineTransform;

import scala.Tuple2;
import oscar.visual.shapes.*;

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
        double angle = Math.atan2(line().y2-line().y1, line().x2-line().x1);
        tx.translate(line().x2, line().y2);
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
			
			arrow.dest_$eq(new Tuple2(100.0, 100.0));
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

	}
	
	
}
