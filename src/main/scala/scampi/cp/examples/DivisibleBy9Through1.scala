package scampi.cp.examples

/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      Hakan Kjellerstrand (hakank@gmail.com), Cedric Druck, Pierre Schaus
 ******************************************************************************/
import scampi.cp.modeling._
import scampi.cp.search._
import scampi.cp.core._
import scala.io.Source._
import scala.math._

/**
 * Find a number consisting of 9 digits in which each of the digits from
 * 1 to 9 appears only once. This number must also satisfy these divisibility requirements:
 * 1. The number should be divisible by 9.
 * 2. If the rightmost digit is removed, the remaining number should be divisible by 8.
 * 3. If the rightmost digit of the new number is removed, the remaining number should be divisible by 7.
 * 4. And so on, until there's only one digit (which will necessarily be divisible by 1).
 */
object DivisibleBy9Through1 extends CPModel {

  def main(args: Array[String]) {

      
    val cp = CPSolver()

    val digits = Array.fill(9)(CPVarInt(cp, 1 to 9))
    val numbers  = Array.fill(9)(CPVarInt(cp, 1 to 1000000000))
    val divisors = Array.fill(9)(CPVarInt(cp, 1 to 100000000))

    val coefs = Array(100000000,10000000,1000000,100000,10000,1000,100,10,1)
    
    cp.solve() subjectTo {
      
      cp.add(alldifferent(digits), Strong)
      for (i <- 1 to 9) {
        cp.add(sum(0 until i)(j => digits(j) * coefs.drop(9-i)(j)) == numbers(i-1))
        cp.add(numbers(i-1) == divisors(i-1) * i)
      }
      
    } exploration {
      cp.binaryFirstFail(digits)
      println(digits.mkString(","))
      println(numbers.mkString(","))
    }
    cp.printStats()
    
  }

}