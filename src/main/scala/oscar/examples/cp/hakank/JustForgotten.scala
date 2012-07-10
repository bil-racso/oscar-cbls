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

  Just forgotten puzzle (Enigma 1517) in Oscar.

  From http://www.f1compiler.com/samples/Enigma 201517.f1.html
  """
  Enigma 1517 Bob Walker, New Scientist magazine, October 25, 2008.

  Joe was furious when he forgot one of his bank account numbers.
  He remembered that it had all the digits 0 to 9 in some order,
  so he tried the following four sets without success:

  9 4 6 2 1 5 7 8 3 0
  8 6 0 4 3 9 1 2 5 7
  1 6 4 0 2 9 7 8 5 3
  6 8 2 4 3 1 9 0 7 5

  When Joe finally remembered his account number, he realised that
  in each set just four of the digits were in their correct position
  and that, if one knew that, it was possible to work out his
  account number. What was it?
  """
  
  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/

object JustForgotten extends CPModel {

  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    val rows = 4
    val cols = 10

    // The four tries
    val a = Array(Array(9,4,6,2,1,5,7,8,3,0),
                  Array(8,6,0,4,3,9,1,2,5,7),
                  Array(1,6,4,0,2,9,7,8,5,3),
                  Array(6,8,2,4,3,1,9,0,7,5))
    
    //
    // variables
    // 
    val x = Array.fill(cols)(CPVarInt(cp, 0 to 9))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      cp.add(alldifferent(x), Strong)
      for(r <- 0 until rows) {
        cp.add(sum(for{c <- 0 until cols} yield x(c) === a(r)(c)) == 4)
      }


    } exploration {
       
      cp.binary(x)
      // cp.binaryFirstFail(x)
      // cp.binaryMaxDegree(x)

      println("\nSolution:")

      println("x: " + x.mkString(""))
      println("\nThe four tries, where '!' represents a correct digit:")
      for(i <- 0 until rows) {
        for(j <- 0 until cols) {
          var c = " ";
          if (a(i)(j) == x(j).getValue()) {
            c = "!";
          }
          print(a(i)(j) + c + " ")
        }
        println()
      }


      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
