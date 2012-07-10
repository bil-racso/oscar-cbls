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
import scala.util.Random


/**

  Stable Marriage problem in Oscar
 
  Problem and OPL model from Pascal Van Hentenryck
  "The OPL Optimization Programming Language", page 43ff.

  This version randomize the priorities.

  Also, see 
  http://www.comp.rgu.ac.uk/staff/ha/ZCSP/additional_problems/stable_marriage/stable_marriage.pdf

  Thanks to Pierre Schaus for improving this model, but speed and
  readability.

  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
 */
object StableMarriageRandom extends CPModel {


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var n = 5
    var num_to_show = 0 // 0: show all solutions
     
    if (args.length > 0) {
      n = args(0).toInt
    }

    if (args.length > 1) {
      num_to_show = args(1).toInt
    }

    println("n: " + n + " num_to_show: " + num_to_show)

    val Men = 0 until n
    val Women = 0 until n

    // Generate the preferences
    val ll = (1 to n).toList
    val rand = new Random(System.currentTimeMillis());
    val rankWomen = Array.fill(n)( Random.shuffle(ll).toArray )
    val rankMen   = Array.fill(n)( Random.shuffle(ll).toArray )

    println("Generated " + n + " ranks.")

    if (n <= 30) {
      for(i <- 0 until n) {
        println("rankWomen #" + i + ": " + rankWomen(i).mkString(" "))
      }

      for(i <- 0 until n) {
        println("rankMen   #" + i + ": " + rankMen(i).mkString(" "))
      }

      println()
    }
  
     
    //
    // decision variables
    //
    val wife    = Array.tabulate(n)(i => CPVarInt(cp, 0 to n-1))
    val husband = Array.tabulate(n)(i => CPVarInt(cp, 0 to n-1))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      val t1 = System.currentTimeMillis

      for (m <- Men) {
        cp.add(element(husband, wife(m),m))
      }

      for (w <- Women) {
        cp.add(element(wife, husband(w),w))
      }      

      for (m <- Men; w <- Women) { 
        val pref_m = element(rankMen(m),wife(m))      // preference of m for his wife
        val pref_w = element(rankWomen(w),husband(w)) // preference of w for her husband
        
        cp.add((pref_m >>= rankMen(m)(w)) ==> (pref_w <<= rankWomen(w)(m)))
        cp.add((pref_w >>= rankWomen(w)(m)) ==> (pref_m <<= rankMen(m)(w)))         
      }
 
      val t2 = System.currentTimeMillis
      println("Constraints took " + (t2-t1) + "ms");

    } exploration {

       println("Explore...")
       
       cp.binary(wife ++ husband)
       // cp.binaryFirstFail(wife ++ husband)
       // cp.binaryMaxDegree(wife ++ husband)

       println("wife   :" + wife.mkString(""))
       println("husband:" + husband.mkString(""))
       println()

       numSols += 1
       

      if (num_to_show > 0 && numSols >= num_to_show) {
        cp.stop()
      }

     }

     println("\nIt was " + numSols + " solutions.\n")

     cp.printStats()
   }

}
