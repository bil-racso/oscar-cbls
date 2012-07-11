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

/**
 *
 * Nontransitive dice in OscaR.
 *
 * From
 * http://en.wikipedia.org/wiki/Nontransitive_dice
 * """
 * A set of nontransitive dice is a set of dice for which the relation
 * 'is more likely to roll a higher number' is not transitive. See also
 * intransitivity.
 *
 * This situation is similar to that in the game Rock, Paper, Scissors,
 * in which each element has an advantage over one choice and a
 * disadvantage to the other.
 * """
 
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */

object NontransitiveDice extends CPModel {

  // returns the maximum element in t
  def my_max(t: Array[CPVarInt]) : CPVarInt = {
    val cp = t(0).getStore
    val mmax = new CPVarInt(cp, (t(0).getMin() to t(0).getMax()))
    for(i <- 0 to t.length-1) {
      cp.post(mmax >= t(i))
    }
    return mmax
  }


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //
    var m = 3 // number of dice
    var n = 6  // number of sides on each die
    var num_to_show = 1

    if (args.length > 0) {
       m = args(0).toInt
    }

    if (args.length > 1) {
       n = args(1).toInt
    }

    if (args.length > 2) {
       num_to_show = args(2).toInt
    }

    val MRANGE = 0 until m
    val NRANGE = 0 until n

    println("number of dice : " + m)
    println("number of sides: " + n)
    println("num_to_show    : " + num_to_show)


    //
    // Decision variables
    // 
    val dice = Array.fill(m)(Array.fill(n)(CPVarInt(cp, 1 to n*2)))
    val dice_flat = dice.flatten

    val comp = Array.fill(m)(Array.fill(2)(CPVarInt(cp, 0 to n*n)))
    val comp_flat = comp.flatten

    // The following variables are for summaries or objectives
    val gap = Array.fill(m)(CPVarInt(cp, 0 to n*n))
    val gap_sum = sum(gap)

    val max_val = CPVarInt(cp, 0 to n*2) // max of dice_flat
    val max_win = CPVarInt(cp, 0 to n*n)   // max of comp_flat

    // number of occurrences of each value of the dice
    val counts     = Array.tabulate(n*2+1)(i => (CPVarInt(cp, 0 to n*m), i))

    // for labeling
    val all = dice_flat ++ Array(max_val, max_win)

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      cp.add(my_max(dice_flat) == max_val)
      cp.add(my_max(comp_flat) == max_win)

      // Number of occurrences for each number
      cp.add(gcc(dice_flat, counts))

      // Order of the number of each die, lowest first
      for(i <- MRANGE) {
        for(j <- 0 until n-1) {
          cp.add(dice(i)(j) <= dice(i)(j+1))
        }
      }

      // Nontransitivity
      for(i <- MRANGE) {
        cp.add(comp(i)(0) > comp(i)(1))
      }

      // Probability gap
      for(i <- MRANGE) {
        cp.add(gap(i) == comp(i)(0) - comp(i)(1))
        cp.add(gap(i) > 0)
      }
      
      // And now we roll...
      // comp() is the number of wins for (A vs B, B vs A)
      for(d <- MRANGE) {
        val sum1 = sum(for{r1 <- NRANGE
                           r2 <- NRANGE}
                          yield (dice(d % m)(r1) >>= dice((d+1) % m)(r2))
          )
        
        cp.add(comp(d%m)(0) == sum1)
        
        val sum2 = sum(for{r1 <- NRANGE
                              r2 <- NRANGE}
                              yield (dice((d+1) % m)(r1) >>= dice(d % m)(r2))
                        )
        
        cp.add(comp(d%m)(1) == sum2)

      }
        
   } exploration {
        
      // cp.binary(all)
      //cp.binaryFirstFail(all)
      cp.binaryMaxDegree(all)

      println("\ngap_sum: " + gap_sum)
      println("gap: " + gap.mkString(""))
      println("max_val: " + max_val)
      println("max_win: " + max_win)
      println("dice:")
      for(i <- MRANGE) {
         println(dice(i).mkString(""))
      }
      println("comp:")
      for(i <- MRANGE) {
         println(comp(i).mkString(""))
      }
      println()
       
     // println("counts: " + counts.filter(c=>c._1 > 0).mkString(""))

      numSols += 1

      if (num_to_show > 0 && numSols >= num_to_show) {
        cp.stop()
      } 

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
