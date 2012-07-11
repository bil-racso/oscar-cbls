/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

  Langford's number problem in Oscar.

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/prob/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  
  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html
  
  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552
 

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object Langford extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    var k = 4
    if (args.length > 0) {
      k = args(0).toInt
    }

    //
    // variables
    //

    // the digits
    val position = Array.fill(2*k)(CPVarInt(cp, 0 to 2*k-1))

    // channel positions to a solution array
    val solution = Array.tabulate(2*k)(i => CPVarInt(cp, 1 to k))

    //
    // constraints
    //
    var numSols = 0
    cp.solveAll subjectTo {
      
      cp.add(alldifferent(position), Strong)
  
      for(i <- 1 to k) {
        cp.add(position(i+k-1) == (position(i-1) + i+1))
        cp.add(element(solution, position(i-1)) == i)
        cp.add(element(solution, position(k+i-1)) == i)
      }

      // symmetry breaking
      cp.add(solution(0) < solution(2*k-1))


    } exploration {
       
      cp.binaryFirstFail(position)

      // print("solution:" + solution.mkString("") + " ")
      println("position:" + position.mkString(""))

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
