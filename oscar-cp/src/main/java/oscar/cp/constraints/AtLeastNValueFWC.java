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
package oscar.cp.constraints;

import oscar.algo.reversible.ReversibleBool;
import oscar.algo.reversible.ReversibleInt;
import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPIntVar;
import oscar.cp.core.Constraint;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class AtLeastNValueFWC extends Constraint {
	
	private CPIntVar [] x;
	
	private CPIntVar nValueVar;
	
	private ReversibleBool [] isValueUsed; //for each value if it is used or not
	private ReversibleInt nbValueUsed; //number of value used
	private ReversibleInt nbBound; //number of bound variables


	private int min;
	private int max;
	private int valSize;
	
	
	public AtLeastNValueFWC(CPIntVar [] x, CPIntVar nval) {
		super(x[0].store(),"AtLeastNValueFWC");
		this.x = x;
		this.nValueVar = nval;
	}

	@Override
	public CPOutcome setup(CPPropagStrength l) {
	    
	     findValueRange();

	     //initialize trails and counters
	     isValueUsed   = new ReversibleBool[valSize];
	     for (int v = 0; v < valSize; v++) {
	    	 isValueUsed[v] = new ReversibleBool(s());
	    	 isValueUsed[v].setValue(false);
	     }
	     nbValueUsed = new ReversibleInt(s(), 0);
	     nbValueUsed.setValue(0);
	     nbBound = new ReversibleInt(s(), 0);
	     nbBound.setValue(0);
	     	    
	     for (int k = 0; k < x.length; k++) {
	       if (x[k].isBound()) {
	    	 int v = x[k].getValue();
	         nbBound.incr();
	         if (!isValueUsed[v-min].getValue()) {
	           nbValueUsed.incr();
	           isValueUsed[v-min].setValue(true);
	         }
	       }
	     }

	     //update lower bound on the number of values
	     if (nValueVar.updateMin(Math.max(nbValueUsed.getValue(), x.length>0 ? 1:0)) == CPOutcome.Failure) {
	       return CPOutcome.Failure;
	     }

	     //update upper bound on the number of values
	     if (nValueVar.updateMax(nbValueUsed.getValue()+x.length-nbBound.getValue()) == CPOutcome.Failure) {
	       return CPOutcome.Failure;
	     }

	     for (int k=0; k < x.length; k++) {
	       if (!x[k].isBound())
	         x[k].callValBindIdxWhenBind(this,k);
	       	 x[k].callPropagateWhenBind(this,false);
	     }
	     if (!nValueVar.isBound()) {
	       nValueVar.callPropagateWhenBoundsChange(this,false);
	     }

	     int ubNbValueUsed = nbValueUsed.getValue() + (x.length -nbBound.getValue());
	     if(ubNbValueUsed <= nValueVar.getMin()){
	       return prune();
	     }

	     return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome valBindIdx(CPIntVar var, int idx) {
		
		int val = var.getValue();
		nbBound.incr();
		if(!isValueUsed[val-min].getValue()){
			nbValueUsed.incr();
			isValueUsed[val-min].setValue(true);
		}

		int ubNbValueUsed = nbValueUsed.getValue() + (x.length-nbBound.getValue());

		if(nValueVar.updateMin(nbValueUsed.getValue()) == CPOutcome.Failure){
			return CPOutcome.Failure;
		}
		if(nValueVar.updateMax(ubNbValueUsed) == CPOutcome.Failure){
			return CPOutcome.Failure;
		}

		if(ubNbValueUsed == nValueVar.getMin()){
			return prune();
		}

		return CPOutcome.Suspend;
	}
	
	@Override
	public CPOutcome propagate() {
		//_nValueVar has changed
		int ubNbValueUsed = nbValueUsed.getValue() + (x.length - nbBound.getValue());
		if (ubNbValueUsed == nValueVar.getMin()) {
			return prune();
		}
		return CPOutcome.Suspend;
	}
	
	public CPOutcome prune(){
		  //remove used values from unbound variables
		  int [] values = new int[x.length];
		  int nb = 0;
		  for (int k = 0; k < x.length; k++) {
		    if (x[k].isBound()) {
		      values[nb] = x[k].getValue();
		      nb++;
		    }
		  }
		  for (int k = 0; k < x.length; k++) {
		    if (!x[k].isBound()) {
		      for (int i = 0; i < nb; i++) {
		        if (x[k].removeValue(values[i]) == CPOutcome.Failure) {
		          return CPOutcome.Failure;
		        }
		      }
		    }
		  }
		  return CPOutcome.Suspend;
	}
	
	private void findValueRange(){
		min = Integer.MAX_VALUE;
		max = Integer.MIN_VALUE;
		for(int i = 0; i < x.length; i++) {
			min = Math.min(min, x[i].getMin());
			max = Math.max(max, x[i].getMax());
		}
		valSize = max - min + 1;
	}

}
