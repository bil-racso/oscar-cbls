/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.search;

/**
 * Dummy Objective doing nothing (used for feasibility problems)
 * @author pschaus@gmail.com
 */
public class DummyObjective extends Objective {

	@Override
	public void tighten() {		
	}

	@Override
	public void relax() {		
	}

	@Override
	public void setNewBound(int val) {		
	}

	@Override
	public int getBound() {
		return 0;
	}

	@Override
	public int getOptimumBound() {
		return 0;
	}

	@Override
	public boolean isOptimum() {
		return false;
	}
	
	@Override
	public boolean isOK() {
		return true;
	}

}
