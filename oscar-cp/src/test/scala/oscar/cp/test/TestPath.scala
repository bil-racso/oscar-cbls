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

import oscar.cp.modeling._


class TestPath extends FunSuite with ShouldMatchers  {

  val succ = Array(Set(0,1),Set(1,2,3),Set(2,4),Set(3,2,5),Set(4,5),Set(5,0))
  
  // 0 --------> 1 ------>2
  //               \    /\ \
  //               _\/ /   _\/
  //                 3       4
  //                  \     /
  //                   \   /
  //                   _\\/_
  //                     5
  
  /*
  test("Path1") {
      
	  val cp = CPSolver()
	  var X = Array.tabulate(succ.length)(i => CPVarInt(cp,succ(i)))
	  var start = CPVarInt(cp, 0)
	  var end = CPVarInt(cp, 5)
	  var length = CPVarInt(cp, 0 to 5)
	  var nbSol = 0
	  cp.solveAll subjectTo {
        cp.add(path(X,start,end,length))
      } exploration {
        cp.binary(X)
        nbSol += 1
        
      }
      nbSol should be(3)
  }  
  
  test("Path2") {
      
	  val cp = CPSolver()
	  var X = Array.tabulate(succ.length)(i => CPVarInt(cp,succ(i)))
	  var start = CPVarInt(cp, 0)
	  var end = CPVarInt(cp, 5)
	  var length = CPVarInt(cp, 0 to 5)
	  var nbSol = 0
	  cp.solveAll subjectTo {
        cp.add(path(X,start,end,length))
        cp.add(length > 3)
      } exploration {
        cp.binary(X)
        println(X.mkString(","))
        nbSol += 1
      }
      nbSol should be(2)
  }  */ 
  


}
