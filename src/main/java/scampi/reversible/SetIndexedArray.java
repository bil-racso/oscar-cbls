/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.reversible;

public class SetIndexedArray extends AbstractSetIndexedArray {

	public SetIndexedArray(int min, int max) {
		this(min, max, false);
	}
	
	public SetIndexedArray(int min, int max, boolean empty) {
		initIndexes(min, max, empty);
	}

	private int size;
	private int maxV;
	private int minV;
	
	@Override
	protected void createSizeMinMax() {
		size = 0;
		maxV = 0;
		minV = 0;	
	}
	
	@Override
	protected void setSize(int size) {
		this.size = size;
		
	}
	
	@Override
	protected void setMin(int min) {
		this.minV = min;
	}
	
	@Override
	protected void setMax(int max) {
		this.maxV = max;
	}
	
	@Override
	public int getSize() {
		return size;
	}
	
	@Override
	public int getMin() {
		return minV;
	}
	
	@Override
	public int getMax() {
		return maxV;
	}
	
}
