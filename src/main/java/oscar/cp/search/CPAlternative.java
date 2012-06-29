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
 ******************************************************************************/
package oscar.cp.search;

import java.util.ArrayList;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.Constraint;
import oscar.cp.core.Store;
import oscar.search.Alternative;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class CPAlternative extends Alternative{
	
	private ArrayList<Constraint> constraints;
	private ArrayList<Constraint> constraintsOnFailure;
	
	private Store s;
	
	public CPAlternative(String name, Store s) {
		super(name);
		this.s = s;
		constraints = new ArrayList<Constraint>(1);
		constraintsOnFailure = new ArrayList<Constraint>();
	}
	
	public CPAlternative(Store s) {
		this("CPAlternative",s);
	}
	
	public CPAlternative(String name,Store s, Constraint c) {
		this(name,s);
		addConstraint(c);	
	}
	
	public CPAlternative(Store s,Constraint c) {
		this("CPAlternative",s,c);
	}
	
	public void addConstraint(Constraint c) {
		constraints.add(c);
	}
	
	public void addConstraintOnBacktrack(Constraint c) {
		constraintsOnFailure.add(c);
	}
	
	@Override
	public boolean execute() {
		return s.post(constraints) != CPOutcome.Failure;
		
	}
	
	@Override
	public boolean executeOnBacktrack() {
		return s.post(constraintsOnFailure) != CPOutcome.Failure;
	}

}
