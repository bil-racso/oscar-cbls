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
import oscar.search._
import oscar.reversible._
import oscar.cp.core._
import scala.io.Source

/** 
 * Binero is a grid game, similar to Sudoku.
 * You get a 2n x 2n grid which must be filled with ones and zeroes.
 * Some of the symbols are given. Empty squares are represented with -1.
 * - On each line and each column there must be as many zeros as ones.
 * - No more than 2 zeros or ones can be placed on a line or a column consecutively.
 * - Identical columns or lines are forbidden.
 * 
 * @author VictorJavAdore
 * 
 * Input examples :
 * - First line : an integer n, the half dimension of the grid
 * - Next 2n lines : original state of the grid (a dot means a blank square)
 *	5
 *	0.11.....1
 *	..1....0.0
 *	.0..1.1.1.
 *	.1........
 *	1.0..1....
 *	.0..0...11
 *	...1.....1
 *	0..1.1.0..
 *	..0.......
 *	11....11.0
 *
 */
object Binero {
  def main(args: Array[String]): Unit = {
    val cp = CPSolver()
    
    val firstLine::restLines = Source.fromFile("data/binero2.txt").getLines.toList
	val n = firstLine.toInt // The grid's half size
	     
    val range = 0 until 2*n
    val rangeArr = (0 until 2*n).toArray
    
    val origGrid = for(l <- restLines.toArray) // Reading the input grid
      yield l.toArray.map {
        case '0' => 0
        case '1' => 1
        case '.' => -1
      }
    
    val grid = for(i <- rangeArr; j <- rangeArr)  yield CPVarInt(cp, 0 to 1) // The variable grid
    
    // Arrays containing the elements of the lines and columns of the variable grids
    val line = for(i <- rangeArr) yield grid.slice(i*2*n, (i+1)*2*n)
    val column = for(i <- rangeArr) yield (for(j <- rangeArr) yield grid(j*2*n+i))
    
    var numSol = 0
    
    cp.solve() subjectTo {
      // The solution must contain the elements of the input grid
      for(i <- range; j <- range; if(origGrid(i)(j) != -1))
          cp.add(grid(2*n*i+j) == origGrid(i)(j))
      
      for(i <- range) {
        // Each line must contain exactly n zeroes (and ones)
        cp.add(gcc(line(i), 0 to 1, n, n))
        cp.add(gcc(column(i), 0 to 1, n, n))
        // There can't be more than 2 ones or zeroes consecutively
        cp.add(regular(line(i), stretchAutomaton(line(i), 1, 2)))
        cp.add(regular(column(i), stretchAutomaton(column(i), 1, 2)))
        // All lines and all columns must be different
        for(j <- i+1 until 2*n) {
          cp.add(new TabNotEqual(line(i),  line(j), 2*n))
          cp.add(new TabNotEqual(column(i), column(j), 2*n))
        }
      }
    } exploration {
      cp.binaryFirstFail(grid)  
      numSol += 1
      
      // Printing the solution
      for(i <- range) println(grid.slice(2*n*i, 2*n*(i+1)).mkString(" "))
      println
    } run() // find all solutions
    
    println("Number of solutions : "+numSol)
    // Printing some stats
    cp.printStats()
  }
  
  /**
   * Custom constraint which obliges two arrays to be different
   */
  class TabNotEqual(val tab1: Array[CPVarInt], val tab2: Array[CPVarInt], val len: Int) extends Constraint(tab1(0).s) {
    
    val valuesBin = Array(new ReversibleInt(s,0), new ReversibleInt(s,0))
    val numBound = Array(new ReversibleInt(s,0), new ReversibleInt(s,0))
    
    
    override def setup(l: CPPropagStrength): CPOutcome = {
      if(tab1.length != len || tab2.length != len)
        CPOutcome.Success
      
      for ((v,i) <- (tab1++tab2).zipWithIndex) {
        if(v.isBound) {
          val ok = valBindIdx(v,i) 
          if (ok != CPOutcome.Suspend) return ok
        }
        else 
          v.callValBindIdxWhenBind (this,i)
      }
      CPOutcome.Suspend
    }
    
    override def valBindIdx (x: CPVarInt, i: Int): CPOutcome = {
      valuesBin(i/len).value += x.min*intPow(2, i%len)
      numBound(i/len).incr()
      if(numBound(0).value == len && numBound(1).value == len) {
    	  if (valuesBin(0).value == valuesBin(1).value)
    		 CPOutcome.Failure
    	  else
    		 CPOutcome.Success
      }
      else
        CPOutcome.Suspend
    }

    /**
     * Integer power function
     * @param a an integer
     * @param e the exponent
     * @return a^e, (a up to e)
     */
    def intPow(a: Int, e: Int): Int = if(e == 0) 1 else a*intPow(a, e-1)
  }
}
