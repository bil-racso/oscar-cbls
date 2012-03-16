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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Set;

import org.jdesktop.swingx.JXMapViewer;
import org.jdesktop.swingx.mapviewer.DefaultWaypointRenderer;
import org.jdesktop.swingx.mapviewer.GeoPosition;
import org.jdesktop.swingx.mapviewer.Waypoint;
import org.jdesktop.swingx.mapviewer.WaypointRenderer;
import org.jdesktop.swingx.painter.Painter;

/**
 * @author Pierre Schaus
 */
class MapPainter implements Painter<JXMapViewer> {
	Map mymap;
	
	
	private WaypointRenderer renderer = new DefaultWaypointRenderer();

	
	MapPainter(Map map) {
		this.mymap = map;
	}
	
	public void paint(Graphics2D g, JXMapViewer map, int w, int h) {
		g = (Graphics2D) g.create();
		//convert from viewport to world bitmap
		Rectangle rect = mymap.viewer.getViewportBounds();
		g.translate(-rect.x, -rect.y);

		//do the drawing
		g.setColor(Color.RED);
		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		g.setStroke(new BasicStroke(2));
		

		for (MapLine l: mymap.getLines()) {

			//convert geo to world bitmap pixel 
			Point2D pt1 = mymap.viewer.getTileFactory().geoToPixel(new GeoPosition(l.lt1, l.lg1), mymap.viewer.getZoom());
			Point2D pt2 = mymap.viewer.getTileFactory().geoToPixel(new GeoPosition(l.lt2, l.lg2), mymap.viewer.getZoom());

			g.drawLine((int) pt1.getX(), (int) pt1.getY(), (int) pt2.getX(), (int) pt2.getY());

		}
		
		
		g.setColor(Color.BLUE);
		for (Waypoint wp : mymap.getWaypoints()) {
            Point2D pt1 = map.getTileFactory().geoToPixel(wp.getPosition(), map.getZoom());
            int x = (int) pt1.getX();
            int y = (int) pt1.getY();
            
            g.setStroke(new BasicStroke(3f));
            g.setColor(Color.BLUE);
            g.drawOval(x-10,y-10,20,20);
            g.setStroke(new BasicStroke(1f));
            g.drawLine(x-10,y-0,x+10,y+0);
            g.drawLine(x-0,y-10,x+0,y+10);
            
            
            
            
		}
		

		

		g.dispose();
	}
	
	protected void paintWaypoint(final Waypoint w, final Graphics2D g) {
        renderer.paintWaypoint(g, mymap.viewer, w);
    }
}

