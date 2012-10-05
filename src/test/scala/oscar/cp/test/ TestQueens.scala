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

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._
import collection.immutable.SortedSet


import org.scalacheck._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestQueens extends FunSuite with ShouldMatchers  {


  test("Queens") {
    
    
    def nbSol(n: Int,strong: Boolean) = {
           val cp = CPSolver()
           val cons = if (strong) Strong else Weak
           val Queens = 0 until n
           //variables
           val queens = for(i <- Queens) yield CPVarInt(cp,1 to n)
           var nbsol = 0
           cp.solveAll subjectTo {
    	     cp.add(alldifferent(queens),cons)
    	     cp.add(alldifferent(for(i <- Queens) yield queens(i) + i),cons)
    	     cp.add(alldifferent(for(i <- Queens) yield queens(i) - i),cons)
           } exploration {        
             for (q <- Queens.suspendable) {
               cp.branchAll(1 to n)(v => cp.post(queens(q) == v))
             }
             nbsol += 1
           }
           nbsol
    }
    
    nbSol(7,true) should be(40)
    nbSol(7,false) should be(40)
      
    
    
 
    
  }  
  

  


}
