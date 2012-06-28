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
import java.util.Random;

public class VisualUtil {
	
	private static Random rand = new Random(2001);
	
	public static Color[] getRandomColorArray(int n) {
		Color [] myColors =  new Color[n];
		for(int i = 0; i < n; i++){
			//fill our array with random colors
			myColors[i] = getRandomColor();//.brighter();
		}
		return myColors;
	}
	
	private static double next() {
		return rand.nextDouble()*0.5 + 0.4;
		
	}
	
	
	public static Color getRandomColor() {
		return new Color((int)(next() * 255),(int)(next() * 255), (int)(next() * 255));
	}

}
