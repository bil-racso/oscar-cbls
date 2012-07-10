/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com)
 ******************************************************************************/
package oscar.example.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.math._
import Array._


/*

  Golomb Golomb ruler in Oscar.

  CSPLib problem 6
  http://www.cs.st-andrews.ac.uk/~ianm/CSPLib/prob/prob006/index.html
  """
  These problems are said to have many practical applications including 
  sensor placements for x-ray crystallography and radio astronomy. A 
  Golomb ruler may be defined as a set of m integers 
  0 = a_1 < a_2 < ... < a_m such that the m(m-1)/2 differences 
  a_j - a_i, 1 <= i < j <= m are distinct. Such a ruler is said to contain 
  m marks and is of length a_m. The objective is to find optimal (minimum 
  length) or near optimal rulers.

  Note that a symmetry can be removed by adding the constraint that 
  a_2 - a_1 < a_m - a_{m-1}, the first difference is less than the last. 
  """

  Also see:
  * http://mathworld.wolfram.com/GolombRuler.html
  * http://en.wikipedia.org/wiki/Golomb_ruler
  * http://www.research.ibm.com/people/s/shearer/grule.html


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object GolombRuler extends CPModel {

  def maxDomNotbound(vars: Iterable[CPVarInt]): Iterable[(CPVarInt, Int)] = {
    val notbound = vars.filterNot(_.isBound)
    if (notbound.nonEmpty) {
      val sizeMax = notbound.map(_.getSize).max
      notbound.zipWithIndex.filter {
        _._1.getSize == sizeMax
      }
    } else {
      Iterable()
    }
  }
 

  def increasing(cp: CPSolver, y: Array[CPVarInt]) = {
    for (i <- 1 until y.length) {
      cp.add(y(i-1) <= y(i), Strong)
    }
  }

  def main(args: Array[String]) {
    
    val cp = CPSolver()

    //
    // data
    //
    var m = 8

    if (args.length > 0) {
      m = args(0).toInt
    }

    val n = m*m

    //
    // variables
    //
    val mark = Array.fill(m)(CPVarInt(cp, 0 to n))
    val differences = for{i <- 0 until m; j <- i+1 until m} yield mark(j)-mark(i)
                        
    //
    // constraints
    //
    var numSols = 0
    cp.minimize(mark(m-1)) subjectTo {

      cp.add(alldifferent(mark), Strong)
      cp.add(alldifferent(differences), Strong)

      increasing(cp, mark)

      // symmetry breaking
      cp.add(mark(0) == 0)
      cp.add(mark(1)-mark(0) < mark(m-1) - mark(m-2))

      // ensure positive differences 
      // (Cred to Pierre Schaus.)
      differences.foreach(d => cp.add(d > 0))

     } exploration {
 
       // cp.binaryFirstFail(mark) // 1381 backtracks for m=8
       cp.binary(mark) // 756 backtracks for m=8

       println("\nSolution:")
       print("mark: " + mark.mkString(""))
       println("\ndifferences: " + differences.mkString(""))
       println()

       numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")

    cp.printStats()

  }

}
