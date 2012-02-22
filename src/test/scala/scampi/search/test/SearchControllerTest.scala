/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package test.scala.scampi.linprog


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scampi.search._
import scampi.reversible._

class SearchControllerTest extends FunSuite with ShouldMatchers {

   
 
    test("test 1 dfs") {
    			
    	val n = new ReversibleSearchNode()
    	val v = Array.tabulate(3)(i => new ReversibleBool(n))
    	var nb = 0
	
	    n.sc = new IDSSearchController(n,4)
	    
    	n.exploration {
	      n.branch { v(0).value = true } 
    	           { v(0).setValue(false)}
          n.branch { v(1).setValue(true) } 
    	           { v(1).setValue(false)}
          n.branch { v(2).setValue(true) } 
    	           { v(2).setValue(false)}
          println(v.mkString(","))	
          nb += 1
	   }


    	
    }

}

