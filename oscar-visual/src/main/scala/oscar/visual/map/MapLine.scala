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
package oscar.visual.map

import java.awt.Color


/**
 * @author Pierre Schaus
 */
class MapLine(map : VisualMap,  lat1 : Double, long1 : Double, lat2 : Double, long2 : Double, col : Color = Color.RED) {
	private var olat = lat1
	private var olong = long1
	private var dlat = lat2
	private var dlong = long2
	
	var color = col

	def this(map : VisualMap, o: (Double, Double), d: (Double, Double), col : Color = Color.RED) = this(map, o._1, o._2, d._1, d._2, col)
	def remove = map.removeLine(this)
	
	def dest_=(d:(Double,Double)) : Unit = {
		this.dlat = d._1;
		this.dlong = d._2;
		map.viewer.repaint();
	}

	def orig_=(o:(Double,Double) ) : Unit =  {
		this.olat = o._1;
		this.olong = o._2;
		map.viewer.repaint();
	}		
	
	def dest = (dlat, dlong)
	def orig = (olat, olong)
}
