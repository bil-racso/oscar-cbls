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

import java.util.Arrays;
import java.util.Comparator;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarBool;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.cp.core.Store;
import oscar.cp.scheduling.Activity;
import oscar.reversible.ReversibleBool;
import oscar.cp.scheduling.MirrorActivity;


public class UnaryResource extends Constraint {

	private int nbAct;
	private Activity [] activities;
	private CPVarBool [] required;
	private ThetaTree thetaTree;
	private LambdaThetaTree lamdaThetaTree;

	private ActivityWrapper [] wrappers;
	private ActivityWrapper [] ect;
	private ActivityWrapper [] est;
	private ActivityWrapper [] lct;
	private ActivityWrapper [] lst;
	private int [] new_est;

	//mirror activities
	private ActivityWrapper [] mwrappers;
	private ActivityWrapper [] mect;
	private ActivityWrapper [] mest;
	private ActivityWrapper [] mlct;
	private ActivityWrapper [] mlst;
	private int [] new_lct;

	private ESTComparator estComp = new ESTComparator();
	private LSTComparator lstComp = new LSTComparator();
	private ECTComparator ectComp = new ECTComparator();
	private LCTComparator lctComp = new LCTComparator();

	private boolean failure;

	//private CPVarInt [] positions; //position[i] = index of activity in positions i
	//private CPVarInt [] ranks; //rank[i] position of activity i => positions[ranks[i]] == i
	
	private ReversibleBool[] ranked;
	

	public UnaryResource(Activity [] activities, CPVarBool [] required,String name) {
		super(activities[0].start().getStore(),name);
		assert(activities.length == required.length);
		this.name = name;
		this.activities = activities;
		this.required = required;
		this.nbAct = activities.length;

		/*
		positions = CPVarInt.getArray(s, nbAct, 0, nbAct-1,"pos");
		ranks = CPVarInt.getArray(s, nbAct, 0, nbAct-1,"rank");
		*/
		ranked = new ReversibleBool[nbAct];
		for (int i = 0; i < nbAct; i++) {
			ranked[i] = new ReversibleBool(s,false);
		}

		
		this.thetaTree = new ThetaTree(activities.length);
		this.lamdaThetaTree = new LambdaThetaTree(activities.length);

		wrappers = new ActivityWrapper[nbAct];
		ect = new ActivityWrapper[nbAct];
		est = new ActivityWrapper[nbAct];
		lct = new ActivityWrapper[nbAct];
		lst = new ActivityWrapper[nbAct];
		new_est = new int [nbAct];

		//mirror activities
		mwrappers = new ActivityWrapper[nbAct];
		mect  = new ActivityWrapper[nbAct];
		mest = new ActivityWrapper[nbAct]; 
		mlct = new ActivityWrapper[nbAct];
		mlst = new ActivityWrapper[nbAct];
		new_lct = new int [nbAct];

		for (int i = 0; i < activities.length; i++) {
			ActivityWrapper w = new ActivityWrapper(i,activities[i],required[i]);
			wrappers[i] = w;
			ect[i] = w;
			est[i] = w;
			lct[i] = w;
			lst[i] = w;
			new_est[i] = Integer.MIN_VALUE;

			w = new ActivityWrapper(i,new MirrorActivity(activities[i]),required[i]);
			mwrappers[i] = w;
			mect[i] = w;
			mest[i] = w;
			mlct[i] = w;
			mlst[i] = w;
			new_lct[i] = Integer.MAX_VALUE;
		}		

		failure = false;
	}

	public UnaryResource(Activity [] activities, String name) {
		this(activities, makeRequiredArray(activities.length,activities[0].start().getStore()), name);
	}
	
	/**
	 * a number between 0/1 representing the business of the resource over it's horizon
	 * close to 1 means that almost at any point there is an activity executing, close to 0 is the opposite
	 */
	public double getCriticality() {
		int min = Integer.MAX_VALUE;
		int max = Integer.MIN_VALUE;
		int totDur = 0;
		for (int i = 0; i < nbAct; i++) {
			if (required[i].isTrue()) {
				min = Math.min(min, activities[i].est());
				max = Math.max(max, activities[i].lct());
				totDur += activities[i].minDuration();
			}
		}
		return ((double) totDur)/(max-min);
	}
	
	public boolean isRanked() {
		for (int i = 0; i < nbAct; i++) {
			if (required[i].isTrue() && !ranked[i].getValue()) {
				return false;
			}
		}
		return true;
	}
	
	public boolean isRanked(int i) {
		return required[i].isTrue() && ranked[i].getValue();
	}
	
	public void rankFirst(int j) {
		for (int i = 0; i < nbAct; i++) {
			if (i!= j && required[i].isTrue() && !ranked[i].getValue()) {
				s.post(new LeEq(activities[j].end(),activities[i].start()));
			}
		}
		ranked[j].setValue(true);
	}
	
	private static CPVarBool[] makeRequiredArray(int n, Store s) {
		CPVarBool [] res = new CPVarBool[n];
		for (int i = 0; i < res.length; i++) {
			res[i] = new CPVarBool(s);
			s.add(res[i].constraintTrue());
		}
		return res;
	}

	public int getMinTotDur() {
		int d = 0;
		for (int i = 0; i < activities.length; i++) {
			d += activities[i].minDuration();
		}
		return d;
	}

	public Activity [] getActivities() {
		return activities;
	}



	@Override
	protected CPOutcome setup(CPPropagStrength l) {

		
		for (int i = 0; i < nbAct; i++) {
			activities[i].start().callPropagateWhenBoundsChange(this);
			activities[i].end().callPropagateWhenBoundsChange(this);
			
			if (!required[i].isBound()) { // we must do something when an activity becomes required/forbidden 
				//required[i].callValBindIdxWhenBind(this,i);
				required[i].callPropagateWhenBind(this);
			}
		}
		
		for (int i = 0; i < nbAct; i++) {
			for (int j = i+1; j < nbAct; j++) {
				if (required[i].isTrue() && required[j].isTrue()) {
					if (s.post(new Disjunctive(activities[i], activities[j])) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				}
			}
		}
		
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}

		return CPOutcome.Suspend;
	}


	
	@Override
	protected CPOutcome valBindIdx(CPVarInt x, int idx) { 
		if (required[idx].isTrue()) {
			// activity idx is mandatory
			for (int i = 0; i < nbAct; i++) {
				if (i != idx && required[i].isTrue()) {
					if (s.post(new Disjunctive(activities[i], activities[idx])) == CPOutcome.Failure) {
						return CPOutcome.Failure;
					}
				}
			}
		}
		return CPOutcome.Suspend;
	}


	@Override
	protected CPOutcome propagate() {
		//System.out.println("propagate "+getName());	
		failure = false;
		do {
			do {

				do {
					if(!overloadChecking()) {
						return CPOutcome.Failure;
					}
				} while (!failure && detectablePrecedences());
			} while (!failure && notFirstNotLast() && !failure);
		} while (!failure && edgeFinder());
		if (failure) {
			return CPOutcome.Failure;
		} else {
			return CPOutcome.Suspend;
		}

	}



	private void updateEst() {
		Arrays.sort(est, estComp);
		for (int i = 0; i < est.length; i++) {
			est[i].setESTPos(i);
		}
		Arrays.sort(mest, estComp);
		for (int i = 0; i < mest.length; i++) {
			mest[i].setESTPos(i);
		}		
	}


	private boolean overloadChecking() {
		// Init
		updateEst(); // update the activity wrappers such that they now their position according to a non decreasing est sorting

		// One direction
		Arrays.sort(lct, lctComp); // sort activity in non decreasing latest completion time
		thetaTree.reset();
		for (int i = 0; i < nbAct; i++) {
			ActivityWrapper aw = lct[i];
			thetaTree.insert(aw.getActivity(), aw.estPos());
			if (thetaTree.ect() > aw.getActivity().lct()) {
				return false;
			}
		}

		// Other direction
		Arrays.sort(mlct, lctComp);
		thetaTree.reset();
		for (int i = 0; i < nbAct; ++i) {
			ActivityWrapper aw = mlct[i];
			thetaTree.insert(aw.getActivity(), aw.estPos());
			if (thetaTree.ect() > aw.getActivity().lct()) {
				return false;
			}
		}
		return true;
	}


	private boolean detectablePrecedences() {
		// Init
		updateEst();
		// Propagate in one direction
		Arrays.sort(ect, ectComp);
		Arrays.sort(lst, lstComp);
		thetaTree.reset();
		int j = 0;
		for (int i = 0; i < nbAct; i++) {
			ActivityWrapper awi = ect[i];
			if (j < nbAct) {
				ActivityWrapper awj = lst[j];
				while (awi.getActivity().ect() > awj.getActivity().lst()) {
					thetaTree.insert(awj.getActivity(), awj.estPos());
					j++;
					if (j == nbAct)
						break;
					awj = lst[j];
				}
			}
			int esti = awi.getActivity().est();
			boolean inserted = thetaTree.isInserted(awi.estPos());
			if (inserted) {
				thetaTree.remove(awi.estPos());
			}
			int oesti = thetaTree.ect();
			if (inserted) {
				thetaTree.insert(awi.getActivity(), awi.estPos());
			}
			if (oesti > esti) {
				new_est[awi.getIndex()] = oesti;
			} else {
				new_est[awi.getIndex()] = Integer.MIN_VALUE;
			}
		}

		// Propagate in other direction
		Arrays.sort(mect, ectComp);
		thetaTree.reset();
		j = 0;
		for (int i = 0; i < nbAct; ++i) {
			ActivityWrapper awi = mect[i];
			if (j < nbAct) {
				ActivityWrapper awj = mlst[j];
				while (awi.getActivity().ect() > awj.getActivity().lst()) {
					thetaTree.insert(awj.getActivity(), awj.estPos());
					j++;
					if (j == nbAct)
						break;
					awj = mlst[j];
				}
			}
			int lcti = awi.getActivity().est();
			boolean inserted = thetaTree.isInserted(awi.estPos());
			if (inserted) {
				thetaTree.remove(awi.estPos());
			}
			int olcti = thetaTree.ect();
			if (inserted) {
				thetaTree.insert(awi.getActivity(), awi.estPos());
			}
			if (olcti > lcti) {
				new_lct[awi.getIndex()] = -olcti;
			} else {
				new_lct[awi.getIndex()] = Integer.MAX_VALUE;
			}
		}

		// Apply modifications
		boolean modified = false;
		for (int i = 0; i < nbAct; i++) {
			if (new_est[i] != Integer.MIN_VALUE) {
				modified = true;
				if (activities[i].start().updateMin(new_est[i]) == CPOutcome.Failure) {
					failure = true;
				}
			}
			if (new_lct[i] != Integer.MAX_VALUE) {
				modified = true;
				if (activities[i].end().updateMax(new_lct[i]) == CPOutcome.Failure) {
					failure = true;
				}
			}
		}
		return modified;
	}

	private boolean notFirstNotLast() {

		// Init
		updateEst();
		for (int i = 0; i < nbAct; ++i) {
			new_lct[i] = activities[i].lct();
			new_est[i] = activities[i].est();
		}

		// Push in one direction.
		Arrays.sort(lst, lstComp);
		Arrays.sort(lct, lctComp);

		thetaTree.reset();
		int j = 0;

		for (int i = 0; i < nbAct; ++i) {
			ActivityWrapper awi = lct[i];
			while (j < nbAct && awi.getActivity().lct() > lst[j].getActivity().lst()) {
				if (j > 0 && thetaTree.ect() > lst[j].getActivity().lst()) {
					new_lct[lst[j].getIndex()] = lst[j - 1].getActivity().lst();
				}
				thetaTree.insert(lst[j].getActivity(), lst[j].estPos());
				j++;
			}
			boolean inserted = thetaTree.isInserted(awi.estPos());
			if (inserted) {
				thetaTree.remove(awi.estPos());
			}
			int ect_theta_less_i = thetaTree.ect();
			if (inserted) {
				thetaTree.insert(awi.getActivity(), awi.estPos());
			}
			if (ect_theta_less_i > awi.getActivity().lct() && j > 0) {
				new_lct[awi.getIndex()] = Math.min(new_lct[awi.getIndex()], lst[j - 1].getActivity().lct());
			}
		}

		// Push in other direction.
		Arrays.sort(mlst,lstComp);
		Arrays.sort(mlct,lctComp);
		thetaTree.reset();
		j = 0;
		for (int i = 0; i < nbAct; i++) {
			ActivityWrapper awi = mlct[i];
			while (j < nbAct && awi.getActivity().lct() > mlst[j].getActivity().lst()) {
				if (j > 0 && thetaTree.ect() > mlst[j].getActivity().lst()) {
					new_est[mlst[j].getIndex()] = -mlst[j - 1].getActivity().lst();
				}
				thetaTree.insert(mlst[j].getActivity(), mlst[j].estPos());
				j++;
			}
			boolean inserted = thetaTree.isInserted(awi.estPos());
			if (inserted) {
				thetaTree.remove(awi.estPos());
			}
			int mect_theta_less_i = thetaTree.ect();
			if (inserted) {
				thetaTree.insert(awi.getActivity(), awi.estPos());
			}
			if (mect_theta_less_i > awi.getActivity().lct() && j > 0) {
				new_est[awi.getIndex()] = Math.max(new_est[awi.getIndex()], -mlst[j - 1].getActivity().lct());
			}
		}

		// Apply modifications
		boolean modified = false;
		for (int i = 0; i < nbAct; ++i) {
			if (activities[i].lct() > new_lct[i] || activities[i].est() < new_est[i]) {
				modified = true;

				if (activities[i].start().updateMin(new_est[i]) == CPOutcome.Failure) {
					failure = true;
				}

				if (activities[i].end().updateMax(new_lct[i]) == CPOutcome.Failure) {
					failure = true;
				}
			}
		}
		return modified;

	} //end of notFirstNotLast



	private boolean edgeFinder() {
		// Init
		updateEst();
		for (int i = 0; i < nbAct; i++) {
			new_est[i] = activities[i].est();
			new_lct[i] = activities[i].lct();
		}

		// Push in one direction.
		Arrays.sort(lct,lctComp);
		lamdaThetaTree.reset();
		for (int i = 0; i < nbAct; i++) {

			lamdaThetaTree.insert(est[i].getActivity(), i);
		}
		int j = nbAct - 1;
		ActivityWrapper awj = lct[j];
		do {
			lamdaThetaTree.grey(awj.estPos());
			if (--j < 0) {
				break;
			}
			awj = lct[j];
			if (lamdaThetaTree.ect() > awj.getActivity().lct()) {
				failure = true;  // Resource is overloaded
				return false;
			}
			while (lamdaThetaTree.getECT_OPT() > awj.getActivity().lct()) {
				int i = lamdaThetaTree.getResponsible_ECT();
				assert(i >= 0);
				int act_i = est[i].getIndex();
				if (lamdaThetaTree.ect() > new_est[act_i]) {
					new_est[act_i] = lamdaThetaTree.ect();
				}
				lamdaThetaTree.remove(i);
			}
		} while (j >= 0);

		// Push in other direction.
		Arrays.sort(mlct,lctComp);
		lamdaThetaTree.reset();
		for (int i = 0; i < nbAct; ++i) {
			lamdaThetaTree.insert(mest[i].getActivity(), i);
		}
		j = nbAct - 1;
		awj = mlct[j];
		do {
			lamdaThetaTree.grey(awj.estPos());
			if (--j < 0) {
				break;
			}
			awj = mlct[j];
			if (lamdaThetaTree.ect() > awj.getActivity().lct()) {
				failure = true;  // Resource is overloaded
				return false;
			}
			while (lamdaThetaTree.getECT_OPT() > awj.getActivity().lct()) {
				int i = lamdaThetaTree.getResponsible_ECT();
				assert(i >= 0);
				int act_i = mest[i].getIndex();
				if (-lamdaThetaTree.ect() < new_lct[act_i]) {
					new_lct[act_i] = -lamdaThetaTree.ect();
				}
				lamdaThetaTree.remove(i);
			}
		} while (j >= 0);

		// Apply modifications.
		boolean modified = false;
		for (int i = 0; i < nbAct; i++) {
			if (activities[i].est() < new_est[i]) {
				modified = true;
				if (activities[i].start().updateMin(new_est[i]) == CPOutcome.Failure) {
					failure = true;
					return false;
				}
			}

			if (activities[i].lct() > new_lct[i] ) {
				modified = true;
				if (activities[i].end().updateMax(new_lct[i]) == CPOutcome.Failure) {
					failure = true;
					return false;
				}
			}


		}
		return modified;
	}

}


class ActivityWrapper {

	CPVarBool required;
	Activity act;
	int index;
	int est_pos;

	protected ActivityWrapper(int index, Activity act, CPVarBool required) {
		this.act = act;
		this.required = required;
		this.index = index;
		this.est_pos = -1;
	}
	
	/**
	 * @return true if the activity may be possibly be scheduled on this resource
	 */
	public boolean isOptional() {
		return !required.isBound();
	}
	
	/**
	 * @return true if the activity must be scheduled on this resource
	 */
	public boolean isMandatory() {
		return required.isTrue();
	}
	
	/**
	 * @return true if the activity cannot be scheduled on this resource
	 */
	public boolean isForbidden() {
		return required.isFalse();
	}

	Activity getActivity() {
		return act;
	}

	int getIndex() {
		//System.out.println("->"+index);
		return index;
	}

	int estPos() {
		return est_pos;
	}

	void setESTPos(int pos) {
		est_pos = pos;
	}	
}


class ESTComparator implements Comparator<ActivityWrapper> {
	public int compare(ActivityWrapper act0, ActivityWrapper act1) {
		return act0.getActivity().est()-act1.getActivity().est();
	}
}

class LCTComparator implements Comparator<ActivityWrapper> {
	public int compare(ActivityWrapper act0, ActivityWrapper act1) {
		int lct0 = act0.getActivity().lct();
		int lct1 = act1.getActivity().lct();
		if (act0.isOptional()) {
			lct0 = Integer.MAX_VALUE;
		}
		if (act1.isOptional()) {
			lct1 = Integer.MAX_VALUE;
		}
		return lct0 - lct1;
	}
}

class LSTComparator implements Comparator<ActivityWrapper> {
	public int compare(ActivityWrapper act0, ActivityWrapper act1) {
		return act0.getActivity().lct()-act1.getActivity().lct();
	}
}

class ECTComparator implements Comparator<ActivityWrapper> {
	public int compare(ActivityWrapper act0, ActivityWrapper act1) {
		return act0.getActivity().ect()-act1.getActivity().ect();
	}
}
