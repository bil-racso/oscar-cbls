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
 * Integer wrapper
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Int {

    private int val;

    public Int(int v) {
    	this.val = v;
    }


    public int getValue() {
        return val;
    }
    
    public void setValue(int v) {
    	this.val = v;
    }
}
