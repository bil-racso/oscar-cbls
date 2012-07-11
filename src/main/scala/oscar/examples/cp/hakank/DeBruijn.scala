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
import scala.math.pow

/**
 *
 * de Bruijn sequences in Oscar.
 *
 * Implementation of de Bruijn sequences , both 'classical' and 'arbitrary'.
 * The 'arbitrary' version is when the length of the sequence
 * (variable m here) is < base**n.
 *
 *
 * Compare with the the web based programs:
 *    http://www.hakank.org/comb/debruijn.cgi   
 *    http://www.hakank.org/comb/debruijn_arb.cgi
 *
 * @author Hakan Kjellerstrand hakank@gmail.com
 * http://www.hakank.org/oscar/
 *
 */
object DeBruijn extends CPModel {

  // channeling between IntVar array t <=> IntVar s
  def toNum(t: Array[CPVarInt], base: Int=10) = sum(
      Array.tabulate(t.length)(i=> t(i)*pow(base, t.length-i-1).toInt))


  // ensure that the minimum element is first in the array t
  def my_minimum1(cp: CPSolver, t: Array[CPVarInt]) = {
    val min = new CPVarInt(cp, t(0).getMin() to t(0).getMax())
    for(i <- 0 to t.length-1) {
      cp.add(min <= t(i))
    }
    cp.add(t(0) == min)
  }

  // returns the minimum element in t
  def my_min(cp: CPSolver, t: Array[CPVarInt]) : CPVarInt = {
    val min = CPVarInt(cp, t(0).getMin() to t(0).getMax())
    for(i <- 0 to t.length-1) {
      cp.post(min <= t(i))
    }
    return min
  }

  def my_min2(t: Array[CPVarInt]) : CPVarInt = {
    val cp = t(0).getStore
    val mmin = new CPVarInt(cp, (t(0).getMin() to t(0).getMax()))
    for(i <- 0 to t.length-1) {
      cp.post(mmin <= t(i))
    }
    return mmin
  }


  def main(args: Array[String]) {

    val cp = CPSolver()

    // data
    var base = 2
    var n = 3
    var m = pow(base, n).toInt
      
    if (args.length > 0) {
     base = args(0).toInt
    }

    if (args.length > 1) {
      n = args(1).toInt
    }

    if (args.length > 2) {
      m = args(2).toInt
    } else {
      m = pow(base, n).toInt
    }

    println("base: " + base + " n: " + n + " m: " + m)

    //
    // variables
    //
    // (Improvements from the original version suggested by Pierre Schaus.)
    val x        = Array.fill(m)(CPVarInt(cp, 0 to pow(base, n).toInt - 1))
    val binary   = Array.fill(m)(
                               Array.fill(n)(CPVarInt(cp, 0 to base-1)))
    val bin_code = Array.fill(m)(CPVarInt(cp, 0 to base-1))
    val gccv     = Array.tabulate(base)(i => (CPVarInt(cp, 0 to m), i))

    //
    // constraints
    //
    var numSols = 0

    cp.solveAll subjectTo {

      cp.add(alldifferent(x), Strong)
      // channeling x <-> binary
      for (i <- 0 until m) {
         val t = Array.tabulate(n)(j=> CPVarInt(cp, 0 to base-1))
         cp.add(x(i) == toNum(t, base))
         for (j <- 0 until n) {
            cp.add(binary(i)(j) == t(j))
         }
       }


       // the de Bruijn condition
       // the first elements in binary[i] is the same as the last
       // elements in binary[i-i]
       for (i <- 1 until m; j <- 1 until n) {
         cp.add(binary(i-1)(j) == binary(i)(j-1))
       }

       // and around the corner
       for (j <- 1 until n) {
         cp.add(binary(m-1)(j) == binary(0)(j-1))
       }


       // convert binary -> bin_code (de Bruijn sequence)
       for (i <- 0 until m) {
         cp.add(bin_code(i) == binary(i)(0))
       }
     
       // gcc on the de Bruijn sequence
       cp.add(gcc(bin_code, gccv))

       // symmetry breaking
       // (don't know how to do this in Oscar)
       // cp.add(min(x) == x(0)) // TODO!

       // some decompositions (though they require that cp as argument)
       // my_minimum1(cp, x) // This works
       // cp.add(my_min(cp, x) == x(0)) // this works
       cp.add(my_min2(x) == x(0))

     } exploration {
       
       cp.binaryFirstFail(x)

       println("x: " + x.mkString(""))
       println("de Bruijn sequence:" + bin_code.mkString("")) 
       println("gcc:" + gccv.mkString(""))
       println()

       numSols += 1
       
     }
     println("\nIt was " + numSols + " solutions.")

     cp.printStats()
   }

}
