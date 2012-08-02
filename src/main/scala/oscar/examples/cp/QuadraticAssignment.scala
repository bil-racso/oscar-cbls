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


import scala.io.Source
import java.lang._

/**
 * Quadratic Assignment Problem:
 * There are a set of n facilities and a set of n locations.
 * For each pair of locations, a distance is specified and
 * for each pair of facilities a weight or flow is specified
 * (e.g., the amount of supplies transported between the two facilities).
 * The problem is to assign all facilities to different locations
 * with the goal of minimizing the sum of the distances multiplied by the corresponding flows.
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object QuadraticAssignment {
  def main(args: Array[String]) {

    // Read the data
    var lines = Source.fromFile("data/qap.txt").getLines.toList.filter(_ != "")
    val n = lines.head.toInt
    val N = 0 until n
    lines = lines.drop(1)
    var w: Array[Array[Int]] = Array() //weight matrix
    var d: Array[Array[Int]] = Array() //distance matrix
    for (i <- N) {
      w = w :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }
    for (i <- N) {
      d = d :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }

    // State the model and solve it
    val cp = CPSolver()
    val x = N map (v => CPVarInt(cp, 0 until n))
    val D = Array.tabulate(n, n)((i, j) => element(d, x(i), x(j))) //matrix of variables representing the distances

    cp.minimize(sum(N, N)((i, j) => D(i)(j) * w(i)(j))) subjectTo {
      cp.add(alldifferent(x), Strong)
    } exploration {
        while (!allBounds(x)) {
           val (y,i) = minDomNotbound(x).first
           val v = y.min
    	   cp.branch {
             cp.post(y == v)
           } {
             cp.post(y !=v)
           }
        }
        println("solution"+x.mkString(",")) 
    }

    // Print some statistics
    cp.printStats()
  }

}
