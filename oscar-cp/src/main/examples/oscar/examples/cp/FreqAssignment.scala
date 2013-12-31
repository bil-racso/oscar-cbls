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


/**
 * Frequency Allocation Problem:
 * the goal is to allocate frequencies to a number of transmitters so that there is 
 * no interference between transmitters.
 * 
 * There are two types of interferences:
 * * intra-cell interferences: distance between two transmitters freq within a cell >= 16
 * * inter-cell interferences: minimum distance between two transmitters from different cells vary according
 *                             to geographical situation (described in a matrix).
 * @author Pierre Schaus pschaus@gmail.com
 */
object FreqAssignment extends App {
		
	   val nbCells = 25
	   val nbFreq = 256
	   val trans = Array( 8, 6, 6, 1, 4, 4, 8, 8, 8, 8, 4, 9, 8, 4, 4, 10, 8, 9, 8, 4, 5, 4, 8, 1, 1 )
	   val distance = Array(Array( 16,1,1,0,0,0,0,0,1,1,1,1,1,2,2,1,1,0,0,0,2,2,1,1,1 ),
		                    Array( 1,16,2,0,0,0,0,0,2,2,1,1,1,2,2,1,1,0,0,0,0,0,0,0,0 ),
		                    Array( 1,2,16,0,0,0,0,0,2,2,1,1,1,2,2,1,1,0,0,0,0,0,0,0,0 ),
		                    Array( 0,0,0,16,2,2,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1 ),
		                    Array( 0,0,0,2,16,2,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1 ),
		                    Array( 0,0,0,2,2,16,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1 ),
	    	                Array( 0,0,0,0,0,0,16,2,0,0,1,1,1,0,0,1,1,1,1,2,0,0,0,1,1 ),
		                    Array( 0,0,0,0,0,0,2,16,0,0,1,1,1,0,0,1,1,1,1,2,0,0,0,1,1 ),
		                    Array( 1,2,2,0,0,0,0,0,16,2,2,2,2,2,2,1,1,1,1,1,1,1,0,1,1 ),
		                    Array( 1,2,2,0,0,0,0,0,2,16,2,2,2,2,2,1,1,1,1,1,1,1,0,1,1 ),
		                    Array( 1,1,1,0,0,0,1,1,2,2,16,2,2,2,2,2,2,1,1,2,1,1,0,1,1 ),
    		                Array( 1,1,1,0,0,0,1,1,2,2,2,16,2,2,2,2,2,1,1,2,1,1,0,1,1 ),
	    	                Array( 1,1,1,0,0,0,1,1,2,2,2,2,16,2,2,2,2,1,1,2,1,1,0,1,1 ),
		                    Array( 2,2,2,0,0,0,0,0,2,2,2,2,2,16,2,1,1,1,1,1,1,1,1,1,1 ),
		                    Array( 2,2,2,0,0,0,0,0,2,2,2,2,2,2,16,1,1,1,1,1,1,1,1,1,1 ),
		                    Array( 1,1,1,0,0,0,1,1,1,1,2,2,2,1,1,16,2,2,2,1,2,2,1,2,2 ),
		                    Array( 1,1,1,0,0,0,1,1,1,1,2,2,2,1,1,2,16,2,2,1,2,2,1,2,2 ),
	    	                Array( 0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,2,2,16,2,2,1,1,0,2,2 ),
		                    Array( 0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,16,2,1,1,0,2,2 ),
		                    Array( 0,0,0,1,1,1,2,2,1,1,2,2,2,1,1,1,1,2,2,16,1,1,0,1,1 ),
		                    Array( 2,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,1,1,1,16,2,1,2,2 ),
		                    Array( 2,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,1,1,1,2,16,1,2,2 ),
		                    Array( 1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,1,1,16,1,1 ),
    		                Array( 1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,1,2,2,1,16,2 ),
	    	                Array( 1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,1,2,2,1,2,16 ))
		
       val cp = new CPSolver()
	   val freq = Array.tabulate(nbCells)(c => Array.tabulate(trans(c))(t => CPVarInt(0 to nbFreq)(cp)))
	   
	   
	   cp.solve subjectTo {
	     
	     for (c <- 0 until nbCells; t1 <- 0 until trans(c); t2 <- t1+1 until trans(c)) {
	    	 cp.add((freq(c)(t1)-freq(c)(t2)).abs >= distance(c)(c))
	     }
		 println("intra cell constraints added")
		 for (c1 <- 0 until nbCells; c2 <- c1+1 until nbCells; t1 <- 0 until trans(c1); t2 <- 0 until trans(c2)) {
	    	 cp.add((freq(c1)(t1)-freq(c2)(t2)).abs >= distance(c1)(c2))
	     }
		 println("inter cell constraints added")
	     
	   } search {
	     binaryMaxDegree(freq.flatten.toSeq)
	   } onSolution {
	     freq.foreach { c =>
	       c.foreach(print(_))
	       println()
	     }	     
	   }	   
	   println(cp.start(1))
	
	
}
