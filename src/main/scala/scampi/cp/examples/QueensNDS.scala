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
 * Using Non Deterministic Search
 * @author Pierre Schaus pschaus@gmail.com
 */
object QueensNDS  extends CPModel {
	def main(args: Array[String]) {
		
      val cp = CPSolver()
      
      val n = 8 //number of queens
      val Queens = 0 until n
      //variables
      val queens = for(i <- Queens) yield CPVarInt(cp,1 to n)
      
      var nbsol = 0
      
      // true if every queens are bounds
      def allBounds = queens.map(_.isBound()).foldLeft(true)((a,b) => a & b)
      
      cp.solveAll subjectTo {
    	  cp.add(alldifferent(queens),Strong)
    	  cp.add(alldifferent(for(i <- Queens) yield queens(i) + i),Strong)
    	  cp.add(alldifferent(for(i <- Queens) yield queens(i) - i),Strong)
      } exploration {
        while (!allBounds && !cp.isFailed()) {
    	   val q = Queens.filter(!queens(_).isBound).first
           val v = queens(q).getMin()
    	   cp.branch {
             cp.post(queens(q) == v)
           } {
             cp.post(queens(q) !=v)
           }
        }
        if (!cp.isFailed()) {
    	  nbsol += 1
          println("solution"+queens.mkString(","))
        }  
      }
  
      //print some statistics
      println("#sol",nbsol)
    }
}