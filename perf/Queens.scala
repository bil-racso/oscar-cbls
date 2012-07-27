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



import oscar.cp.modeling._
import oscar.search._





/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * Using Non Deterministic Search
 * @author Pierre Schaus pschaus@gmail.com
 */
object Queens  {
	def main(args: Array[String]) {
		
      val cp = CPSolver()
      
      val n = 12 //number of queens
      val Queens = 0 until n
      //variables
      val queens = for(i <- Queens) yield CPVarInt(cp,1 to n)
      
      var nbsol = 0
      cp.solveAll subjectTo {
    	  cp.add(alldifferent(queens),Strong)
    	  cp.add(alldifferent(for(i <- Queens) yield queens(i) + i),Strong)
    	  cp.add(alldifferent(for(i <- Queens) yield queens(i) - i),Strong)
      } exploration {        
        for (q <- Queens.suspendable) {
          cp.branchAll(1 to n)(v => cp.post(queens(q) == v))
        }
        nbsol += 1
      }
  
      //print some statistics
      println("#sol",nbsol)
      cp.printStats()
      
	}
}

