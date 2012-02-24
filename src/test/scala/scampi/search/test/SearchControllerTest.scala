/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package test.scala.scampi.search


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scampi.search._
import scampi.reversible._

class SearchControllerTest extends FunSuite with ShouldMatchers {

   
 
    test("test 1 dfs") {
    			
    	val n = new ReversibleSearchNode()
    	val v = Array.tabulate(3)(i => new ReversibleBool(n))
	
	    n.sc = new DFSSearchController(n)
	    
    	implicit def rev2Bool(r:ReversibleBool): Int = if (r.value == true) 1 else 0
    	
    	var sol = Array[Int]()
    	
    	n.exploration {
	      n.branch { v(0).value = false } 
    	           { v(0).value = true  }
	      n.branch { v(1).value = false } 
    	           { v(1).value = true  }
	      n.branch { v(2).value = false } 
    	           { v(2).value = true  }
          sol = sol :+ v(0)*4+v(1)*2+v(2)
	   }
    	
    	sol should equal(Array(0,1,2,3,4,5,6,7))
    	
    	


    	
    }

}

