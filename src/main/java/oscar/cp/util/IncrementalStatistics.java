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
package oscar.cp.util;

/**
 * Class to maintain average and variance incrementally
 * @author pschaus@gmail.com
 *
 */
public class IncrementalStatistics {
	
	int n;
	double s; // maintain sum_i xi
	double s2; // maintain sum_i xi^2
	
	double variance; // maintain sum_i (xi-s/n)^2
	
	public IncrementalStatistics() {
		n = 0;
		
	}
	
	public void addPoint(double x) {
		
		s += x;
		n++;
		if (n >= 1) {
			variance = s*s / n + x*x - 2 * x * s / n + s2 - 2 * s / n * (s - x);			
		}
		else {
			variance = 0;
		}
		s2 += x*x;
	}
	
	public double getSum() {
		return s;
	}
	
	public double getAverage() {
		assert(n > 0);
		return getSum()/n;
	}
	
	public double getVariance() {
		assert(n > 0);
		return variance/n;
	}

}
