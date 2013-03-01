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

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.reversible.ReversibleInt;


/**
 * Redundant Bin-Packing Flow Constraint
 * @author pschaus@gmail.com
 */
class BinPackingFlowExtended (_x : Array[CPVarInt],_sizes : Array[Int], _l : Array[CPVarInt]) 
	extends BinPackingFlow (_x,_sizes,_l){
	
	val permRev = perm.reverse
  
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

		for (card <- c)
	  	{
			card.callPropagateWhenBoundsChange(this);
	  		card.callPropagateWhenBind(this);
	  	}
			
	  	propagate()
	}

	/**
	 * Adapt the cardinality of bin j
	 * @param j is the bin index
	 * @return Failure if fail detected when adapting cards, or Suspend otherwise
	 */
	 override protected def setCardinality(j : Int) : CPOutcome = {
	  	
	    
	    if (setMinCard(j) == CPOutcome.Failure) 
	      return CPOutcome.Failure
	    
	    
	    if (setMaxCard(j) == CPOutcome.Failure)
	      return CPOutcome.Failure
	    
	    
		return CPOutcome.Suspend
	}
	
	
	/**
	 * compute the maximum cardinality for the bin `bin`
	 */
	def setMaxCard(bin:Int) : CPOutcome =
	{
			
					
		val (card,load) = getCard(bin, perm, (binLoad, nextItemSize) => binLoad + nextItemSize <= l(bin).getMax.intValue())
		//println(" ++" + bin + " " + c(bin).getMin + " - " + c(bin).getMax + "  :  " + card)
		c(bin).updateMax(card)
	}
	
	/**
	 * compute the minimum cardinality for the bin `bin`
	 */
	def setMinCard(bin:Int) : CPOutcome =
	{
			
	  	val (card,load) = getCard(bin, permRev, (binLoad, nextItemSize) => l(bin).getMin.intValue() > binLoad)
	  	//println(" --" + bin + " " + c(bin).getMin + " - " + c(bin).getMax + "  :  " + card + " /// "  + c.map(x=>x.getMin+":"+x.getMax).mkString(","))	  	
		if(load < l(bin).getMin.intValue())
		  return CPOutcome.Failure
		else
		  c(bin).updateMin(card)
	}
	
	
	
	/**
	 * compute the minimum cardinality for the bin `bin`
	 * @param sorted items is the list of items with items used first at the beginning
	 * @param continueLoad(binLoad,nextItemSize) a function that return true if the bin should continue to be loaded
	 * @return  
	 */
	def getCard(bin:Int, sortedItems:Array[Int], continueLoad:(Int,Int) => Boolean) : (Int,Int) =
	{
	
		var binCompCard 	= c_t(bin).getValue // the current value of the computation of the cardinality
		var binLoad 		= l_t(bin).getValue
		val othersBins 		= for (i <- 0 until c_t.length if i!= bin ) yield i
		
		var candidatesAvailableForBin = Array.tabulate(c_t.length){
			b => 	if(b == bin) 0 
					else candidates(b).length - (c(b).getMin.intValue - c_t(b).getValue)  
		}
	 	
		for(i <- sortedItems if x(i).hasValue(bin) && continueLoad(binLoad, sizes(i)))
		{	
			val refuteItem = othersBins.exists(b=> x(i).hasValue(b) 
			    && candidatesAvailableForBin(b)<=0)
			
		
			if (!refuteItem){	  
			  binLoad 		+= sizes(i)
			  binCompCard 	+= 1
			  candidatesAvailableForBin = candidatesAvailableForBin.zipWithIndex.map{
			    case (c,b) => if(x(i).hasValue(b)) (c-1) else c 
			  }
			  
			}
		}
		
		(binCompCard, binLoad)
	}
	
	def candidates(bin:Int) = x.filter(v => v.hasValue(bin))

}
