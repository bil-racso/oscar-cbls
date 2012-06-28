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
 * @author Pierre Schaus pschaus@gmail.com
 */
public abstract class Alternative {
	
	
	private String name = "alternative";
	
	public Alternative(String name) {
		this.name = name;
	}

    public Alternative() {
		this.name = name;
	}

	public abstract boolean execute();
	
	public boolean executeOnBacktrack() {
		return true;
	}
	
	@Override
	public String toString() {
		return name;
	}

}
