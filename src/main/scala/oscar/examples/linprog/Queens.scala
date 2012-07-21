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

package oscar.examples.linprog


import oscar.linprog.modeling._
import oscar.linprog._

/**
 *  The Queens Problem is to place as many queens as possible on the 8x8
 *  (or more generally, nxn) chess board in a way that they do not fight
 *  each other. This problem is probably as old as the chess game itself,
 *  and thus its origin is not known, but it is known that Gauss studied
 *  this problem. 
 *  @author Pierre Schaus pschaus@gmail.com
 */
object Queens extends MIPModel{
	
  def main(args: Array[String]): Unit = {  
    val n = 8
	val Lines = 0 until n
	val Columns = 0 until n
	val mip = MIPSolver()
    val x = Array.tabulate(n,n) ((l,c) => MIPVar(mip,"x"+(l,c), 0 to 1))

    mip.maximize(sum(Lines,Columns) {(l,c) => x(l)(c)} ) subjectTo {
      
      /* at most one queen can be placed in each row */
      for(l <- Lines)
        mip.add(sum(Columns)(c => x(l)(c)) <= 1)
        
      /* at most one queen can be placed in each column */
      for(c <- Columns)
        mip.add(sum(Lines)(l => x(l)(c)) <= 1)

      /* at most one queen can be placed in each "/"-diagonal  upper half*/
      for(i <- 1 until n)        
        mip.add(sum(0 to i)((j) => x(i-j)(j)) <= 1)
        
      /* at most one queen can be placed in each "/"-diagonal  lower half*/
      for(i <- 1 until n)  
        mip.add(sum(i until n)((j) => x(j)(n-1-j+i)) <= 1)     
       
      /* at most one queen can be placed in each "/"-diagonal  upper half*/
      for(i <- 0 until n)  
        mip.add(sum(0 until n-i)((j) => x(j)(j+i)) <= 1)
        
       /* at most one queen can be placed in each "/"-diagonal  lower half*/ 
      for(i <- 1 until n)
        mip.add(sum(0 until n-i)((j) => x(j+i)(j)) <= 1)

    }
    
	println("objective: "+mip.getObjectiveValue())

	for(i <- 0 until n){
	  for(j <- 0 until n)
	    if(x(i)(j).getValue==1) print("Q") else print(".")
	  println()
	}
	mip.release()

  }
}
