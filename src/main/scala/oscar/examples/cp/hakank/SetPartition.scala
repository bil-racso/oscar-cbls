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

  Set partition problem in Oscar.

  Problem formulation from
  http://www.koalog.com/resources/samples/PartitionProblem.java.html
  """
  This is a partition problem.
  Given the set S = {1, 2, ..., n},
  it consists in finding two sets A and B such that:

   A U B = S,
   |A| = |B|,
   sum(A) = sum(B),
   sum_squares(A) = sum_squares(B)
  """

  Note: This model uses a binary matrix to represent the sets.
  

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object SetPartition extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var n = 16
    var num_sets = 2

    if (args.length > 0) {
      n = args(0).toInt
    }

    if (args.length > 1) {
      num_sets = args(1).toInt
    }

    println("n: " + n + " num_sets: " + num_sets)

    //
    // variables
    // 
    // The matrix
    val a = Array.fill(num_sets)(Array.fill(n)(CPVarInt(cp, 0 to 1)))


    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {
    
      for(i <- 0 until num_sets) {
        for(j <- 0 until num_sets if i!=j) {
          cp.add(
                 sum(for{k <- 0 until n} yield a(i)(k)*a(j)(k)) == 0
                 )
          
        }
      }

      // ensure that all integers is in
      // (exactly) one partition
      cp.add(
             sum(
                 for{i <- 0 until num_sets
                     j <- 0 until n} yield a(i)(j)
                 ) == n
             )

      

      for(i <- 0 until num_sets; j <- 0 until num_sets if i < j) {
        // same cardinality
        cp.add(
               sum(for{k <- 0 until n} yield a(i)(k) ) 
               ==
               sum(for{k <- 0 until n} yield a(j)(k) )
               )

        // same sum
        cp.add(
               sum(for{k <- 0 until n} yield a(i)(k)*k ) 
               ==
               sum(for{k <- 0 until n} yield a(j)(k)*k)
               )


        // same sum squared
        cp.add(
               sum(for{k <- 0 until n} yield a(i)(k)*k*a(i)(k)*k ) 
               ==
               sum(for{k <- 0 until n} yield a(j)(k)*k*a(j)(k)*k)
               )
      }

      // symmetry breaking for num_sets == 2
      if (num_sets == 2) {
        cp.add(a(0)(0) == 1)
      }


    } exploration {
       
      cp.binary(a.flatten)
      // cp.binaryFirstFail(a.flatten)
      // cp.binaryMaxDegree(a.flatten)

      println("\nSolution:")

      var sums = 0
      var sums_squared = 0
      for(i <- 0 until num_sets) {
        for(j <- 0 until n if a(i)(j).getValue() == 1) {
          print((j+1) + " ")
          if (i == 0) {
            val v = (j+1)*a(i)(j).getValue()
            sums += v
            sums_squared += v*v
          }
        }
        println()
      }

      println("Sums: " + sums  + " Sums squared: " + sums_squared)

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
