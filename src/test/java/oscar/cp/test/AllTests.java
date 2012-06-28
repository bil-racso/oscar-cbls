/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.test;


import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for test");
		//$JUnit-BEGIN$
        suite.addTestSuite(TestArrayUtils.class);

		//$JUnit-END$
		return suite;
	}

}
