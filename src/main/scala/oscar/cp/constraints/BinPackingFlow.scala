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
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.cp.util.ArrayUtils;
import oscar.reversible.ReversibleInt;

/**
 * Redundant Bin-Packing Flow Constraint
 * @author pschaus@gmail.com
 */
class BinPackingFlow (val x : Array[CPVarInt],val sizes : Array[Int], val l : Array[CPVarInt]) 
	extends Constraint(x(0).s,"BinPackingFlow"){
	
	val c 		= Array.tabulate(l.length)(i => CPVarInt(s,0,sizes.length)) //cardinalities
	protected val l_t		= Array.tabulate(l.length)(i => new ReversibleInt(s,0)) // keep track of the current load of each bin
	protected val c_t 	= Array.tabulate(l.length)(i => new ReversibleInt(s,0)) // keep track of the number of items in each bin

	
	protected val perm = ArrayUtils.sortPerm(sizes) //permutation of sorted items i.e. s[perm[i]] <= s[perm[i+1]]
	

	override def setup( strength : CPPropagStrength) : CPOutcome =  {
	  
		if( 	x.exists(_.updateMax(l.length-1) == CPOutcome.Failure)
		    ||  x.exists(_.updateMin(0) == CPOutcome.Failure)
		    || 	s.post(new GCCVar(x, 0, c), CPPropagStrength.Strong) == CPOutcome.Failure)
		{

		    CPOutcome.Failure
		}

		for (lt <- l) 
			lt.callPropagateWhenBoundsChange(this);
		
		for ((xt,i) <- x.zipWithIndex) {
			if (xt.isBound) {
				val j = xt.getValue
				l_t(j).setValue(l_t(j).getValue + sizes(j))
				c_t(j).incr
			}
			else {
				xt.callValBindIdxWhenBind(this,i);
				xt.callPropagateWhenBind(this);
			}
		}

		propagate()
	}
	

	override def valBindIdx(x : CPVarInt, idx : Int ) : CPOutcome = {
		val j 		= x.getValue
		val size 	= sizes(idx)
		l_t(j).setValue(l_t(j).getValue + size)
		c_t(j).incr()

	    CPOutcome.Suspend
	}
	
	override def propagate() : CPOutcome =  {
	  //no call to super???
		
		if((0 until l.length).exists(setCardinality(_) == CPOutcome.Failure)) CPOutcome.Failure
		else 
		  {

			CPOutcome.Suspend
		  
		  }
	}
	
	/**
	 * Adapt the cardinality of bin j
	 * @param j is the bin index
	 * @return Failure if fail detected when adapting cards, or Suspend otherwise
	 */
	protected def setCardinality(j : Int) : CPOutcome = {
	  
	    val minVal = l(j).getMin
	    val maxVal = l(j).getMax
	    
	    //how many items do I need at least to reach minVal ?
	    var v = l_t(j).getValue
	    var i = x.length-1;
	    var nbAdded = 0;
	    while (v < minVal && i >= 0){
	      if (!x(perm(i)).isBound && x(perm(i)).hasValue(j)) {
	        print(sizes(perm(i)) + " ")
	        v += sizes(perm(i))
	        nbAdded += 1
	      }
	      i -= 1
	    }
	    if(v < minVal) {
	      //println("Failure 1")
	      return CPOutcome.Failure; //not possible to reach the minimum level
	    }
	    val nbMin = nbAdded + c_t(j).getValue;
println(" --" +j + " " + c(j).getMin + " - " + c(j).getMax + "  :  " + nbMin+ " /// " + c.map(x=>x.getMin+":"+x.getMax).mkString(","))	    
	    if (c(j).updateMin(nbMin) == CPOutcome.Failure){
	      //println(j + " " + nbMin )
	      
	      //println("Failure 2")
	      return CPOutcome.Failure
	    }
	    // how many items can I use at most before reaching maxVal ?
	    v = l_t(j).getValue();
	    i = 0;
	    nbAdded = 0;
	    while (i < x.length && v+sizes(perm(i)) <= maxVal) {
	      if (!x(perm(i)).isBound && x(perm(i)).hasValue(j)) {
	        print(sizes(perm(i)) + " ")
	        v += sizes(perm(i))
	        nbAdded += 1
	      }
	      i += 1
	    }
	    val nbMax = nbAdded + c_t(j).getValue
println(" ++" +j + " " + c(j).getMin + " - " + c(j).getMax + "  :  " + nbMax)	    
	    if (c(j).updateMax(nbMax) == CPOutcome.Failure){
	     // println("Failure 3")
	      return CPOutcome.Failure
	    }

		return CPOutcome.Suspend
	}

}
