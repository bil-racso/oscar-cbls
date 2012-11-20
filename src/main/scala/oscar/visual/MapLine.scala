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


/**
 * @author Pierre Schaus
 */
class MapLine(map : VisualMap,  lat1 : Double, long1 : Double, lat2 : Double, long2 : Double) {
	var lt1 = lat1
	var lg1 = long1
	var lt2 = lat2
	var lg2 = long2

	
	def setDest(lt2 : Double, lg2 : Double) {
		this.lt2 = lt2;
		this.lg2 = lg2;
		map.viewer.repaint();
	}

	def setOrig(lt1 : Double ,lg1 : Double ) {
		this.lt1 = lt1;
		this.lg1 = lg1;
		map.viewer.repaint();
	}		
}
