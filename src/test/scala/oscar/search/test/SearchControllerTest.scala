/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.search.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.search._
import oscar.reversible._
import oscar.cp.modeling._
import oscar.cp.core.CPVarInt

class SearchControllerTest extends FunSuite with ShouldMatchers with CPModel {

   
 
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
    
    test("test 2 dfs") {
    			
    	val cp = CPSolver()
    	val x = Array.fill(2)(CPVarInt(cp,1 to 2))
    	val y = Array.fill(2)(CPVarInt(cp,1 to 2))
	
    	def dom(x: CPVarInt) = (x.getMin() to x.getMax()).filter(x.hasValue(_))
    	
    	var nbSol = 0
    	cp.exploration {
        	while (! allBounds(x)) {
    		 val i = x.indices.find(!x(_).isBound()).get	    
    		 cp.branchAll(dom(x(i)))(v => cp.post(x(i) == v))
    		 cp.branchAll(dom(y(i)))(v => cp.post(y(i) == v))      
    	    }
        	nbSol += 1
    	}
    	nbSol should equal(16)
    }

}

