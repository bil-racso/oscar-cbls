/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.search._
import scala.util.continuations._
import oscar.reversible.ReversibleBool
import oscar.search.IDSSearchController
import oscar.reversible.ReversibleInt



/**
 * 
 * Test Non deterministic search
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object TestNDS  extends CPModel {
  def main(args: Array[String])  {

      
		
	val cp = CPSolver()
	val v = Array.tabulate(3)(i => new ReversibleBool(cp))
	
	val bcr = new ReversibleInt(cp,0)
	
	var nb = 0
	
	//cp.solve
	//cp.sc = new IDSSearchController(cp,4)
	cp.exploration {
	  cp.branch { v(0).value = true } 
    	        { v(0).value = false}
      cp.branch { v(1).value = true 
                  bcr.value = 3} 
    	        { v(1).value = false}
      cp.branch { v(2).value = true } 
    	        { v(2).value = false}
      println(v.mkString(",")+" "+bcr.getValue())	
      nb += 1
	}
	
	
	/*
	cp.exploration {		
	     loop(0 until v.length) {
    	     i => cp.branch { v(i).setValue(true) } 
    	                    { v(i).setValue(false)}
         }
	     nb += 1
	     println("solution:"+v.mkString(","))
	}*/
	println("nb:"+nb)
	
	



  }

}
