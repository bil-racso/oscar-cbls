/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.algo.search._
import oscar.cp.core._

/**
 * Create a number using only the digits 4,4,3,3,2,2,1 and 1.
 * So it can only be eight digits.
 * You have to make sure the ones are separated by one digit,
 *                     the twos are separated by two digits
 *                     the threes are separated with three digits and
 *                     the fours are separated by four digits
 * @author Pierre Schaus pschaus@gmail.com
 */
object MagicNumber extends App {

  val cp = CPSolver()

  var one = CPVarInt(0 to 7)(cp)
  var two = CPVarInt(0 to 7)(cp)
  var three = CPVarInt(0 to 7)(cp)
  var four = CPVarInt(0 to 7)(cp)
  var x = Array.fill(8)(CPVarInt(1 to 4)(cp))

  cp.solve subjectTo {
    cp.add(x(one) == 1)
    cp.add(x(one + 2) == 1)
    cp.add(x(two) == 2)
    cp.add(x(two + 3) == 2)
    cp.add(x(three) == 3)
    cp.add(x(three + 4) == 3)
    cp.add(x(four) == 4)
    cp.add(x(four + 5) == 4)
    cp.add(gcc(x, 1 to 4, 2, 2))
    cp.add(one < two)
  } search {
    binaryFirstFail(Seq(one, two, three, four))
  } onSolution {
    println(x.mkString((",")))  
  }
  println(cp.start())

}
