/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._

/**
 * A number with an interesting property:
 *
 * When I divide it by  2, the remainder is 1.
 * When I divide it by  3, the remainder is 2.
 * When I divide it by  4, the remainder is 3.
 * When I divide it by  5, the remainder is 4.
 * When I divide it by  6, the remainder is 5.
 * When I divide it by  7, the remainder is 6.
 * When I divide it by  8, the remainder is 7.
 * When I divide it by  9, the remainder is 8.
 * When I divide it by 10, the remainder is 9.
 *
 * It's not a small number, but it's not really big, either.
 * When I looked for a smaller number with this property I couldn't find one.
 * Can you find it?
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object MagicModuloNumber extends App {

  val cp = CPSolver()

  var x = CPVarInt(cp, 0 to 10000)

  cp.solve subjectTo {
    for (i <- 2 to 10) {
      cp.add(modulo(x, i, i - 1))
    }
  } exploration {
    cp.binaryFirstFail(Array(x))
    println(x)
  } run()
  cp.printStats()

}
