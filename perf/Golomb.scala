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

import oscar.cp._
import scala.io.Source._
import scala.math._
import Array._
import oscar.cp.constraints.Minus


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
object Golomb {
  
  def increasing(cp: CPSolver, y: Array[CPIntVar]) = {
    for (i <- 1 until y.length) {
      cp.add(y(i-1) <= y(i), Strong)
    }
  }  
  
  def main(args: Array[String]) {

    val cp = CPSolver()
    cp.silent = true

    //
    // data
    //
    var m = 11

    if (args.length > 0) {
      m = args(0).toInt
    }

    val n = m*m

    //
    // variables
    //
    val mark = Array.fill(m)(CPIntVar(0 to n)(cp))
    
    val differences = for{i <- 0 until m; j <- i+1 until m} yield {
      val d = mark(j)-mark(i) // inefficient because it does not create a sparse one      
      cp.add(d >= (j-i)*(j-i+1)/2)
      d
    }

    //
    // constraints
    //
    var numSols = 0
    cp.minimize(mark(m - 1))

    cp.add(allDifferent(mark), Medium)
    cp.add(allDifferent(differences), Medium)

    increasing(cp, mark)

    // symmetry breaking
    cp.add(mark(0) == 0)
    //cp.add(mark(1)-mark(0) < mark(m-1) - mark(m-2))
    cp.add(differences(0) <= differences((m * m - m) / 2 - 1))

    // ensure positive differences 
    // (Cred to Pierre Schaus.)
    differences.foreach(d => cp.add(d > 0))

    cp.search {
 
       binaryStatic(mark,_.min) // 756 backtracks for m=8
     
     } onSolution {
       
       println("\nSolution:")
       print("mark: " + mark.mkString(""))
       println("\ndifferences: " + differences.mkString(""))
       println()

       numSols += 1

   }

   println(cp.start())

  }
}


