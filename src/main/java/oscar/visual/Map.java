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

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.swing.JInternalFrame;
import javax.swing.JPanel;

import org.jdesktop.swingx.JXMapKit;
import org.jdesktop.swingx.JXMapKit.DefaultProviders;
import org.jdesktop.swingx.JXMapViewer;
import org.jdesktop.swingx.mapviewer.DefaultTileFactory;
import org.jdesktop.swingx.mapviewer.DefaultWaypointRenderer;
import org.jdesktop.swingx.mapviewer.GeoPosition;
import org.jdesktop.swingx.mapviewer.TileFactoryInfo;
import org.jdesktop.swingx.mapviewer.Waypoint;
import org.jdesktop.swingx.mapviewer.WaypointPainter;
import org.jdesktop.swingx.mapviewer.WaypointRenderer;
import org.jdesktop.swingx.painter.Painter;

/**
 * @author Pierre Schaus
 */
public class Map extends JPanel {


    
	public JXMapViewer viewer = new JXMapViewer();

	public ArrayList<MapLine> lines;
	public ArrayList<Waypoint> waypoints;
	



	public Map() {
		super(new BorderLayout());
		this.lines = new ArrayList<MapLine>();
		this.waypoints = new ArrayList<Waypoint>();
		final int max = 17;
		TileFactoryInfo info = new TileFactoryInfo(1,max-2,max,
				256, true, true, // tile size is 256 and x/y orientation is normal
				"http://tile.openstreetmap.org",
				"x","y","z") {
			public String getTileUrl(int x, int y, int zoom) {
				zoom = max-zoom;
				String url = this.baseURL +"/"+zoom+"/"+x+"/"+y+".png";
				return url;
			}

		};
		DefaultTileFactory tf = new DefaultTileFactory(info);
		viewer.setTileFactory(tf);
		viewer.setZoom(11);
		viewer.setAddressLocation(new GeoPosition(51.5,0));


		viewer.setOverlayPainter(new MapPainter(this));



		viewer.setName("mapKit");
		viewer.setPreferredSize(new java.awt.Dimension(413, 218));
		//viewer.setDefaultProvider(DefaultProviders.OpenStreetMaps);
		add(viewer);
	}
	
	public Waypoint createWaypoint(double lt, double lg) {
		Waypoint res = new Waypoint(lt,lg);
		waypoints.add(res);
		refresh();
		return res;
	}
	
	
	public MapLine createLine(double lt1, double lg1, double lt2, double lg2) {
		MapLine l = new MapLine(this,lt1,lg1,lt2,lg2);
		lines.add(l);

		refresh();
		
		return l;
	}
	
	public List<Waypoint> getWaypoints() {
		return waypoints;
	}

	public List<MapLine> getLines() {
		return lines;
	}

	public void refresh() {
		viewer.repaint();
	}

	public static void main(String[] args) {

		VisualFrame f = new VisualFrame("toto");
		JInternalFrame inf = f.createFrame("tmap");

		Map map = new Map();
		try {
			String [] countries = new String[]{"Albania", "Andorra", "Austria", "Belgium", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Ireland", "Latvia", "Luxembourg", "Portugal", "Spain" , "United Kingdom", };
	        for (String c: countries) {
	        	
	        	Location loc = Geocoder.getLocation(c);
	        	System.out.println(c+" "+loc.lat+","+loc.lon);
	        	map.createWaypoint(loc.lat,loc.lon);
	        	try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
	        	
	        }
			
			
			Location ny = Geocoder.getLocation("New York");
			Location be = Geocoder.getLocation("Brussels");
			
			//MapLine l = map.createLine(ny.lat,ny.lon,be.lat,be.lon);

			
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		
		
		

		map.refresh();

		inf.add(map);
		f.pack();
		System.out.println(map.hashCode());
		
		try {
			Thread.sleep(2000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

	}

}


