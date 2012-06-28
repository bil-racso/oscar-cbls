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

/**
 * @author Pierre Schaus
 */
public class Location {
	public double lon, lat;

	public Location (Double lat, Double lon) {
		this.lon = lon;
		this.lat = lat;
	}
	
	public Location (String lat, String lon) {
		this.lon = Double.parseDouble(lon);
		this.lat = Double.parseDouble(lat);
	}

	public String toString () { return "Lat: "+lat+", Lon: "+lon; }


	public double distance(Location loc) {

		double lat1 = lat;  double lon1 = lon; double lat2 = loc.lat ; double lon2 = loc.lon;

		double theta = lon1 - lon2;
		double dist = Math.sin(deg2rad(lat1)) * Math.sin(deg2rad(lat2)) + Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) * Math.cos(deg2rad(theta));
		dist = Math.acos(dist);
		dist = rad2deg(dist);
		dist = dist * 60 * 1.1515;

		dist = dist * 1.609344;

		return dist;
	}


	private double deg2rad(double deg) {
		return (deg * Math.PI / 180.0);
	}


	private double rad2deg(double rad) {
		return (rad * 180.0 / Math.PI);
	}

}

