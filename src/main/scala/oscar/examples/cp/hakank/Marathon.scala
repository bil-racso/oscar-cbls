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

/**
 *
 * Marathon puzzle in Oscar
 *
 * """
 * From Xpress example
 * http://www.dashoptimization.com/home/cgi-bin/example.pl?id=mosel_puzzle_5_3
 * """
 * Dominique, Ignace, Naren, Olivier, Philippe, and Pascal
 * have arrived as the first six at the Paris marathon.
 * Reconstruct their arrival order from the following
 * information:
 * a) Olivier has not arrived last
 * b) Dominique, Pascal and Ignace have arrived before Naren
 *    and Olivier
 * c) Dominique who was third last year has improved this year.
 * d) Philippe is among the first four.
 * e) Ignace has arrived neither in second nor third position.
 * f) Pascal has beaten Naren by three positions.
 * g) Neither Ignace nor Dominique are on the fourth position.
 *
 * (c) 2002 Dash Associates
 * author: S. Heipcke, Mar. 2002
 * """
 * 
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Marathon extends CPModel {

   // 
   // Decomposition of inverse constraint
   // 
   // Channel of positions of x and y:
   //    j == x(i) <=> y(j) == i
   // 
   // Here: 
   //   x is the position array
   //   y are the placements
   // 
   def inverse(cp: CPSolver, x: Array[CPVarInt], y: Array[CPVarInt]) {
      val len = x.length
      for(i <- 0 until len;
          j <- 0 until len) {
        cp.add( (y(j) === i) === (x(i) === j) )
      }
   }



   def main(args: Array[String]) {

      val cp = CPSolver()

      //
      // data
      // 
      val n = 6
      val runners_str = Array("Dominique", "Ignace", "Naren",
                              "Olivier", "Philippe", "Pascal")

      //
      // decision variables
      // 

      // Note: in order to use inverse(), the runners and places are in the domain 0..n-1
      val runners = Array.fill(n)(CPVarInt(cp, 0 to n-1))
      val places = Array.fill(n)(CPVarInt(cp, 0 to n-1))
      val Array(dominique, ignace, naren, olivier, philippe, pascal) = runners

      var numSols = 0
      cp.solveAll() subjectTo {

	cp.add(alldifferent(runners), Strong)

        // a: Olivier not last
        cp.add(olivier != n)
        
        // b: Dominique, Pascal and Ignace before Naren and Olivier
        cp.add(dominique  < naren)
        cp.add(dominique  < olivier)
        cp.add(pascal     < naren)
        cp.add(pascal     < olivier)
        cp.add(ignace     < naren)
        cp.add(ignace     < olivier)

        // c: Dominique better than third
        cp.add(dominique  < 2)
        
        // d: Philippe is among the first four
        cp.add(philippe   <= 3)
        
        // e: Ignace neither second nor third
        cp.add(ignace     != 1)
        cp.add(ignace     != 2)

        // f: Pascal three places earlier than Naren
        cp.add(pascal + 3 == naren)

        // g: Neither Ignace nor Dominique on fourth position
        cp.add(ignace     != 3)
        cp.add(dominique  != 3)

        inverse(cp, runners, places)

      } exploration {

        cp.binary(runners ++ places)

        println("Runners: " ++ runners.mkString(""))
        println("Places:")
        for(p <- 0 until n) {
          println("Place " + (p+1) + ": " + runners_str(places(p).getValue()))
        }
        println()

        numSols += 1
      }

      println("\nIt was " + numSols + " solutions.")	  
      cp.printStats()

  }

}
