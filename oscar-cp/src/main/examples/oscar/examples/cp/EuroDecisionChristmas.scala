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
import oscar.cp.core._
import oscar.visual._
import java.awt.Color

/**
 * Try to maximize the number of children receiving a gifts given the number of gifts they asked
 * and the capacity of Santa (28 gifts).
 * @author Pierre Schaus pschaus@gmail.com
 */
object EuroDecisionChistmas extends App {
  val names = Array("Fulkerson", "Dijkstra", "Benders", "Dantzig", "Konig")
  val weights = Array(10, 8, 5, 5, 15) // number of gifts
  val profit = Array(2, 3, 0, 1, 5) // number of children

  val cp = CPSolver()

  val x = Array.fill(names.size)(CPVarBool(cp))
  val obj = CPVarInt(cp, 0 to profit.sum)

  cp.maximize(obj) subjectTo {
    cp.add(binaryKnapsack(x, profit, weights, obj, CPVarInt(cp, 0 to 28)))
  } search {
    binaryStatic(x)
  } onSolution {
    println("objective:" + obj.value)
    for (i <- 0 until names.size; if (x(i).value == 1)) {
      println(names(i))
    }
  } start ()

}
