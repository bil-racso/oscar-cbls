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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._



class TestSearchNew extends FunSuite with ShouldMatchers  {

  test("ids search, bug #36") {
    val cp = new CPSolver()
    
    var nbSol = 0
    cp.onSolution {nbSol += 1}
    
    
    val x = Array(CPVarInt(cp, 0))

    cp.minimize(x(0)) subjectTo() search {
      new BinaryFirstFailBranching(x)
    } 
    val stat = cp.start()
    stat.nbSols should be(1)
    nbSol should be(1)
  }

  test("explocompleted") {
    val cp = CPSolver()
    
    var nbSol = 0
    cp.onSolution {nbSol += 1}
    
    val x = Array.tabulate(3)(i => CPVarBool(cp))
    
    cp.search {
      new BinaryStaticOrderBranching(x)
    } 
    
    cp.start (nbSolMax = 3).completed should be(false)
    cp.start ().completed should be(true)
    cp.start(nbSolMax = 3).completed should be(false)
    cp.start().completed should be(true)
    cp.start(failureLimit = 3).completed should be(false)
  }
  
  test("timelimit") {
	  val cp = CPSolver()
	  val x = Array.fill(40)(CPVarInt(cp,0 to 1))
	  
	  var t0 = System.currentTimeMillis()
	  cp.solve subjectTo { } search {
	    new BinaryStaticOrderBranching(x)

	  } 
	  
	  val stat = cp.start(timeLimit = 2)
	  val time: Int = ((System.currentTimeMillis()-t0)/1000).toInt
	  time should be >=(2)
	  stat.completed should be(false)
	  time should be <=(4)
	  stat.completed should be(false)
  }
  
  test("optimize") {

		val cp = new CPSolver();
		val x = CPVarInt(cp,Array(1,5,9,10));
		cp.minimize(x) subjectTo {}
		
		var best = 0
		cp.onSolution { best = x.value }
		
		cp.search {
			new BinaryStaticOrderBranching(Array(x),_.max)
		}
		val stat = cp.start()
		stat.nbSols should be(4)
		best should be(1)
		
	
  } 
  
    test("test 2 dfs") {
    			
    	val cp = CPSolver()
    	val x = Array.fill(2)(CPVarInt(cp,1 to 2))
    	val y = Array.fill(2)(CPVarInt(cp,1 to 2))
	
    	//def dom(x: CPVarInt) = (x.min to x.max).filter(x.hasValue(_))
    	
    	var nbSol = 0
    	cp.onSolution { nbSol += 1}
    	
    	cp.search {
    		new BinaryFirstFailBranching(x) ++ new BinaryFirstFailBranching(y)
    	}
    	
    	val stat = cp.start()
    	nbSol should equal(16)
    	stat.nbSols should be(16)
    }  
  
  

  
    
  
 


}
