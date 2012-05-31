/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.constraints;

import java.util.Set;

import scampi.cp.core.CPOutcome;
import scampi.cp.core.CPPropagStrength;
import scampi.cp.core.Constraint;
import scampi.cp.core.CPVarInt;

/**
 * Regular Constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Regular extends Constraint {

	private int nbStates;
	private int [][] T; //transition matrix;
	private int initialState;
	private Set<Integer> acceptingStates;
	
	private CPVarInt [] x;
	private CPVarInt [] q;

    /**
     * Constraint x to be a valid sequence accepted by the automaton
     * @param x
     * @param automaton
     */
	public Regular(CPVarInt [] x, Automaton automaton) {
		super(x[0].getStore(),"Regular");
		this.x = x;

	    automaton.setPosted();//ensure that the automaton is not modified anymore
	    nbStates = automaton.getNbStates();
	    T = automaton.getTransitionMatrix();//transition matrix
	    initialState = automaton.getInitialState();
	    acceptingStates = automaton.getAcceptingStates();
	    q = new CPVarInt[x.length];
	    for (int i = 0; i < q.length; i++) {
			q[i] = new CPVarInt(s,0,nbStates-1);
		}		
	}

	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (s.post(new ElementCst2D(T,new CPVarInt(s,initialState,initialState),x[0],q[0])) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		
		for (int v = 0; v < nbStates; v++) {
			if (!acceptingStates.contains(v)) {
				if (q[x.length-1].removeValue(v) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
		}
		
		for (int i = 1; i < x.length; i++) {
			if (s.post(new ElementCst2D(T,q[i-1],x[i],q[i])) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		
		return CPOutcome.Success;
	}

}
