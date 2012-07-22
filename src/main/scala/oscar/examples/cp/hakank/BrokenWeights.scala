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
package oscar.examples.cp.hakank

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
import scala.io.Source._
import scala.math._

/*

  Clock triplet problem in Oscar.

  Problem formulation
  http://www.f1compiler.com/samples/Dean 20Clark 27s 20Problem.f1.html
  """
  Dean Clark's Problem (Clock Triplets Problem)
 
  The problem was originally posed by Dean Clark and then presented
  to a larger audience by Martin Gardner. 
 
  The problem was discussed in Dr. Dobbs's Journal, May 2004 in an article 
  by Timothy Rolfe. According to the article, in his August 1986 column for 
  Isaac Asimov's Science Fiction Magazine, Martin Gardner presented this problem:
  
    Now for a curious little combinatorial puzzle involving the twelve
    numbers on the face of a clock. Can you rearrange the numbers (keeping
    them in a circle) so no triplet of adjacent numbers has a sum higher 
    than 21? This is the smallest value that the highest sum of a triplet
    can have.
  """


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object BrokenWeights extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var m = 40
    var n = 4

    if (args.length > 0) {
      m = args(0).toInt
    }

    if (args.length > 1) {
      n = args(1).toInt
    }


    //
    // variables
    //
    val weights = Array.fill(n)(CPVarInt(cp, 1 to m))
    val x   = Array.fill(m, n)(CPVarInt(cp, -1 to 1))
    val x_flat = x.flatten

    //
    // constraints
    //
    var numSols = 0

    cp.minimize(weights(n-1)) subjectTo {

      // total weight of the pieces
      cp.add(sum(weights) == m)

      // ensure that all weighst are handled
      for(i <- 0 until m) {
        cp.add(sum(for(j <- 0 until n) yield x(i)(j)*weights(j)) == i+1)
      }

      // symmetry breaking
      for(j <- 1 until n) {
        cp.add(weights(j-1) < weights(j))
      }

      
    } exploration {
       
      cp.binaryMaxDegree(weights ++ x_flat)

      println("\nSolution:")
      println("weights:" + weights.mkString(""))
      for(i <- 0 until m) {
        print("weight " + "%2s".format(i+1) + ": ")
        for(j <- 0 until n) {        
          print("%3s".format(x(i)(j).getValue()) + " ")
        }
        println()
      }

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
