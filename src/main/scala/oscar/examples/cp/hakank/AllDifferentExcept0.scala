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
 * Decomposition of the global constraint alldifferent_except_0 in in Oscar.
 * 
 *  From Global constraint catalogue:
 * http://www.emn.fr/x-info/sdemasse/gccat/Calldifferent_except_0.html
 *  """ 
 * Enforce all variables of the collection VARIABLES to take distinct 
 * values, except those variables that are assigned to 0.
 * 
 * Example
 *    (<5, 0, 1, 9, 0, 3>)
 * 
 * The alldifferent_except_0 constraint holds since all the values 
 * (that are different from 0) 5, 1, 9 and 3 are distinct.
 * """
 *
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object AllDifferentExcept0 extends CPModel {

  // Decomposition of alldifferent_except_0
  def alldifferent_except_0(cp: CPSolver, y: Array[CPVarInt]) = {

    for(i <- 0 until y.length; j <- 0 until i) {
      cp.add( ((y(i) !== 0) && (y(j) !== 0)) ==> (y(i) !== y(j)) )
    }
  }

  // Decomposition of increasing
  def increasing(cp: CPSolver, y: Array[CPVarInt]) = {
    for (i <- 1 until y.length) {
      cp.add(y(i-1) <= y(i))
    }
  }


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    var n = 7

    if (args.length > 0) {
      n = args(0).toInt
    }

    // variables
    val x = Array.fill(n)(CPVarInt(cp, 0 to n))
    val occurrences = Array.tabulate(n+1)(i => (CPVarInt(cp, 0 to n), i))
    val z = occurrences(0)._1  // the tuple is (#occurrences, value)

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      alldifferent_except_0(cp, x)                                

      // Just for fun, we add that x should be increasing
      // increasing(cp, x)

      // There must be exactly 2 0's
      // Perhaps a full gcc is overkill just to fetch the # of 0's...
      cp.add(gcc(x, occurrences))
      cp.add(z == 2)


     } exploration {
       
       cp.binaryFirstFail(x)

       println("x:" + x.mkString(""))
       // println("occurrences:" + occurrences.mkString(""))
       println("z:" + z)
       println()

       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
