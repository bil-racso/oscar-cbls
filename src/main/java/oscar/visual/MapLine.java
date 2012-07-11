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
public class MapLine {
	double lt1, lg1, lt2, lg2;
	Map map;

	public MapLine(Map map,double lt1, double lg1, double lt2, double lg2) {
		this.map = map;
		this.lt1 = lt1;
		this.lg1 = lg1;
		this.lt2 = lt2;
		this.lg2 = lg2;
	}

	public void setDest(double lt2, double lg2) {
		this.lt2 = lt2;
		this.lg2 = lg2;
		map.viewer.repaint();

	}

	public void setOrig(double lt1, double lg1) {
		this.lt1 = lt1;
		this.lg1 = lg1;
		map.viewer.repaint();
	}		
}
