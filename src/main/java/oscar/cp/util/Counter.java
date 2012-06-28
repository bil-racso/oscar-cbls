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
 * Counter class than can be reset and incremented
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Counter {

    private int cpt;

    public Counter() {
        cpt = 0;
    }

    public void reset() {
        cpt = 0;
    }

    public void incr() {
        cpt++;
    }

    public int getValue() {
        return cpt;
    }
}
