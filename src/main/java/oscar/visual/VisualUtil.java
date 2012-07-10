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
