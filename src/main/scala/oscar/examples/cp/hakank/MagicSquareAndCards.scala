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

/*

  Magic squares and cards problem in Oscar.

  Martin Gardner (July 1971)
  """
  Allowing duplicates values, what is the largest constant sum for an order-3
  magic square that can be formed with nine cards from the deck.
  """

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object MagicSquareAndCards extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var n = 3

    if (args.length > 0) {
      n = args(0).toInt
    }

    val RANGE = 0 until n

    val colors = 4
    val values = 13

    //
    // variables
    // 
    val x = Array.fill(n)(Array.fill(n)(CPVarInt(cp, 1 to values)))
    val s = CPVarInt(cp, 1 to values*colors)
    val counts = Array.tabulate(values+1)(i => (CPVarInt(cp, 0 to colors), i))

    //
    // constraints
    //
    var numSols = 0

    cp.maximize(s) subjectTo {

      cp.add(gcc(x.flatten, counts), Strong)

      // the standard magic square constraints 
      // (sans all_different of all elements)
      for(i <- RANGE) {
        // rows
        val row = for{j <- RANGE} yield x(i)(j)
        cp.add( sum(row) == s)
        
        // columns
        val col = for{j <- RANGE} yield x(j)(i)
        cp.add( sum(col) == s)

        cp.add( alldifferent(row), Strong)
        cp.add( alldifferent(col), Strong)

      }

      // diagonals
      cp.add( sum(for{i <- RANGE} yield x(i)(i)) == s)
      cp.add( sum(for{i <- RANGE} yield x(i)(n-i-1)) == s)

      // redundant constraint
      cp.add(sum(counts.map(_._1)) == n*n) 

      // symmetry breaking
      cp.add(x(n-1)(n-1) == values)


    } exploration {
       
      // cp.binary(x.flatten)
      // cp.binaryFirstFail(x.flatten)
      cp.binaryMaxDegree(x.flatten)

      println("\nSolution:")

      println("counts: " + counts.mkString(""))
      for(i <- RANGE) {
        println(x(i).mkString(""))
      }

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
