/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.algo.search.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.algo.search._
import oscar.algo.reversible._
import oscar.algo.search.SearchNode
import oscar.algo.search.DFSSearchController

class SearchControllerTest extends FunSuite with ShouldMatchers  {

   
 
    test("test 1 dfs") {
    			
    	val n = new SearchNode()
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
	   } run()
    	
       sol should equal(Array(0,1,2,3,4,5,6,7))
	
    }
    


}

