/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
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
