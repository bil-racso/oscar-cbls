/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.examples


import scampi.cp.modeling._
import scampi.cp.search._

/**
 * n-queens model: place n-queens on a chess-board such that they don't attack each other.
 * this program search for all the solutions
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object Queens  extends CPModel {
	def main(args: Array[String]) {
		
      val cp = CPSolver()
      
      val n = 6 //number of queens
      val Queens = 0 until n
      //variables
      val queens = for(i <- Queens) yield CPVarInt(cp,1 to n)
      
      var nbsol = 0
      cp.onSolution {
    	  nbsol += 1
      }
      
      cp.solveAll subjectTo {
    	  cp.add(alldifferent(queens))
    	  cp.add(alldifferent(for(i <- Queens) yield queens(i) + i))
    	  cp.add(alldifferent(for(i <- Queens) yield queens(i) - i))
      } exploring {
    	  //exploration of the search tree
    	  new Binary(queens:_*)
      }
  
      //print some statistics
      println("#sol",nbsol)
      cp.printStats()
    }
}