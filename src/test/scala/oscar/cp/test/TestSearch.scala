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
import oscar.search.IDSSearchController
import oscar.cp.modeling._



class TestSearch extends FunSuite with ShouldMatchers  {

  test("ids search, bug #36") {
    val cp = new CPSolver()
    val x = Array(CPVarInt(cp, 0))
    var nbSol = 0
    cp.sc = new IDSSearchController(cp, 4)
    cp.minimize(x(0)) subjectTo() exploration {
      cp.binaryFirstFail(x)
      println(x.mkString(","))
      nbSol += 1
    } run()
    nbSol should be(1)
  }

  test("explocompleted") {
    val cp = CPSolver()
    val v = Array.tabulate(3)(i => false)
    var nb = 0
    cp.exploration {
      cp.branch { v(0) = true } { v(0) = false }
      cp.branch { v(1) = true } { v(1) = false }
      cp.branch { v(2) = true } { v(2) = false }
      println(v.mkString(","))
      nb += 1
    } 
    
    cp.run (nbSolMax = 3)
    cp.explorationCompleted should be(false)
    cp.run ()
    cp.explorationCompleted should be(true)
    cp.run (nbSolMax = 3)
    cp.explorationCompleted should be(false)
    cp.run ()
    cp.explorationCompleted should be(true)
    cp.run (failureLimit = 3)
    cp.explorationCompleted should be(false)
  }
  
  test("timelimit") {
	  val cp = CPSolver()
	  val x = Array.fill(20)(CPVarInt(cp,0 to 1))
	  var nb = 0
	  var t0 = System.currentTimeMillis()
	  cp.solve subjectTo {
	    
	  } exploration {
	    cp.binary(x)
	    //println(x.mkString(","))
	    nb += 1
	  } 
	  cp.run(timeLimit = 1)
	  cp.run(timeLimit = 1)
	  val time: Int = ((System.currentTimeMillis()-t0)/1000).toInt
	  time should be >=(2)
	  cp.explorationCompleted should be(false)
	  time should be <=(3)
	  cp.explorationCompleted should be(false)
  }
  
  test("optimize") {

		val cp = new CPSolver();
		val x = CPVarInt(cp,Array(1,5,9,10));
		cp.minimize(x)
		cp.exploration {
			cp.binary(Array(x),_.max)
			println(x)
			
		}
	
  }   
  
  

  
    
  
 


}
