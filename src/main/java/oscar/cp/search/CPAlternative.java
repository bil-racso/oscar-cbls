/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
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
