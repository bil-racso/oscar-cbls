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
