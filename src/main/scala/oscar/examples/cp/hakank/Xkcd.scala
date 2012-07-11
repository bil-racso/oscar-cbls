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


/**
 *
 * Implementing Xkcd knapsack problem in Oscar
 *
 * http://xkcd.com/287/
 *
 * Some amount (or none) of each dish should be ordered to
 * give a total of exact 15.05
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object Xkcd extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    val price  = Array(215, 275, 335, 355, 420, 580) // in cents
    val num_prices = price.length
    val total = 1505


    // variables
    val x = Array.fill(num_prices)(CPVarInt(cp, 0 to 10))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      cp.add(sum(Array.tabulate(num_prices)(i=> x(i)*price(i))) == total)

     } exploration {
       
       cp.binaryFirstFail(x)

       println("x:" + x.mkString(" "))

       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
