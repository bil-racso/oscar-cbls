/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.search._
import oscar.cp.core._
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
object DivisibleBy9Through1 {

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
