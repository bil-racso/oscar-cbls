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

  Divisible by 9 through 1 problem in Oscar.

  From http://msdn.microsoft.com/en-us/vcsharp/ee957404.aspx
  " Solving Combinatory Problems with LINQ"
  """
  Find a number consisting of 9 digits in which each of the digits 
  from 1 to 9 appears only once. This number must also satisfy these 
  divisibility requirements:
  
   1. The number should be divisible by 9.
   2. If the rightmost digit is removed, the remaining number should 
      be divisible by 8.
   3. If the rightmost digit of the new number is removed, the remaining 
      number should be divisible by 7.
   4. And so on, until there's only one digit (which will necessarily 
      be divisible by 1).
  """
  
  Also, see
  "Intel® Parallel Studio: Great for Serial Code Too (Episode 1)"
  http://software.intel.com/en-us/blogs/2009/12/07/intel-parallel-studio-great-for-serial-code-too-episode-1/


  @author Hakan Kjellerstrand hakank@gmail.com
  http://www.hakank.org/oscar/
 
*/
object DivisibleBy9Through1 extends CPModel {

  // channeling between IntVar array t <=> IntVar s
  def toNum(t: Array[CPVarInt], base: Int=10) = sum(
      Array.tabulate(t.length)(i=> t(i)*pow(base, t.length-i-1).toInt))


  def main(args: Array[String]) {

    val cp = CPSolver()

    //
    // data
    //

    var base = 10

    if (args.length > 0) {
      base = args(0).toInt
    }

    val n = base - 1
    val m = pow(base, n).toInt -1

    println("base: " + base + " n: " + n + " m: " + m)
 
    //
    // variables
    //

    // the digits
    val x = Array.fill(n)(CPVarInt(cp, 1 to n))

    // the numbers. t(0) contains the answer
    val t = Array.fill(n)(CPVarInt(cp, 0 to m))

    //
    // constraints
    //
    var numSols = 0
    cp.solveAll subjectTo {
      
      cp.add(alldifferent(x), Strong)
  
      for(i <- 0 until n) {
        // Force domains
        val lb = pow(base, n-i-1).toInt
        val ub = pow(base, n-i).toInt
        // println("lb: " + lb + " ub: " + ub)
        cp.add(t(i) >= lb)
        cp.add(t(i) <= ub)

        val mm = base-i-1
        cp.add(t(i) == toNum(Array.tabulate(mm)(j=>x(j)), base))
        cp.add(modulo(t(i), mm, 0))
      }


    } exploration {
       
      cp.binaryFirstFail(x)

      println("\nSolution:")
      print("x: " +  x.mkString(""))
      print("\nt: " +  t.mkString(""))
      println("\nNumber: " + t(0) +  " (" + x.mkString("") + " in base " + base + ")")
      println()


      numSols += 1

   }

    println("\nIt was " + numSols + " solutions.")
    cp.printStats()

  }

}
