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

  Olympic puzzle in Oscar.

  Benchmark for Prolog (BProlog)
  """
  File   : olympic.pl
  Author : Neng-Fa ZHOU
  Date   : 1993
  
  Purpose: solve a puzzle taken from Olympic Arithmetic Contest
  
  Given ten variables with the following configuration:
  
                  X7   X8   X9   X10
  
                     X4   X5   X6
  
                        X2   X3
  
                           X1
  
  We already know that X1 is equal to 3 and want to assign each variable
  with a different integer from {1,2,...,10} such that for any three
  variables
                         Xi   Xj
  
                            Xk
  
  the following constraint is satisfied:
  
                       |Xi-Xj| = Xk
  """


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object Olympic extends CPModel {

  def abs_minus(x: CPVarInt,
             y: CPVarInt,
             z: CPVarInt) : Constraint = 
    return z == (x-y).abs()


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val n = 10


    //
    // variables
    //
    val x = Array.fill(n)(CPVarInt(cp, 1 to n))
    val Array(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) = x
      
    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      cp.add(alldifferent(x), Strong)

      cp.add(x1 == 3)

      cp.add(abs_minus(x2, x3, x1))
      cp.add(abs_minus(x4, x5, x2))
      cp.add(abs_minus(x5, x6, x3))
      cp.add(abs_minus(x7, x8, x4))
      cp.add(abs_minus(x8, x9, x5))
      cp.add(abs_minus(x9, x10, x6))


    } exploration {
       
      // cp.binary(x)
      // cp.binaryFirstFail(x)
      cp.binaryMaxDegree(x)

      println("x: " + x.mkString(""))

      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
