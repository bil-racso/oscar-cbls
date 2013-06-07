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

package oscar.cp.core


/**
 * Represent an propagation event (AC5 Events).
 * An propagation event records a fine grained modification to the domain of 
 * a variable with the associated delta (min changed, val removed...)
 * that must be notified to the constraint.
 * @author Pierre Schaus pschaus@gmail.com
 */
abstract class PropagEvent(val cstr: Constraint,val prior: Int = 0) {
	
	/**
	 * Notifies the constraint of the change associated the event and propagate the associated AC5 method
	 * @return The outcome of the propagation
	 */
	def notifyConstraint(): CPOutcome
	

	def propagate(): CPOutcome = {
		if(cstr.isActive()){
			val oc = notifyConstraint();
			if (oc == CPOutcome.Success) {
				cstr.deactivate();
			}
			oc;
		}
		else CPOutcome.Suspend;
	}

}


class PropagEventBindToValue(cstr: Constraint, x: CPVarInt) extends PropagEvent(cstr,cstr.getPriorityBindL1()) {
	def notifyConstraint(): CPOutcome = return cstr.valBind(x)
}

class PropagEventBindToValueIdx(cstr: Constraint, x: CPVarInt, idx: Int) extends PropagEvent(cstr,cstr.getPriorityBindL1()) {
	def notifyConstraint(): CPOutcome = return cstr.valBindIdx(x,idx)
}

class PropagEventRemoveValue(cstr: Constraint, x: CPVarInt, v: Int) extends PropagEvent(cstr,cstr.getPriorityRemoveL1()) {
	def notifyConstraint(): CPOutcome = return cstr.valRemove(x,v)
}

class PropagEventRemoveValueIdx(cstr: Constraint, x: CPVarInt, idx: Int, v: Int) extends PropagEvent(cstr,cstr.getPriorityRemoveL1()) {
	def notifyConstraint(): CPOutcome = return cstr.valRemoveIdx(x,idx,v)
}

class PropagEventUpdateBounds(cstr: Constraint, x: CPVarInt) extends PropagEvent(cstr,cstr.getPriorityBoundsL1()) {
	def notifyConstraint(): CPOutcome = return cstr.updateBounds(x)
}

class PropagEventUpdateBoundsIdx(cstr: Constraint, x: CPVarInt, idx: Int) extends PropagEvent(cstr,cstr.getPriorityBoundsL1()) {
	def notifyConstraint(): CPOutcome = return cstr.updateBoundsIdx(x,idx)
}

class PropagEventUpdateMax(cstr: Constraint, x: CPVarInt, v: Int) extends PropagEvent(cstr,cstr.getPriorityBoundsL1()) {
	def notifyConstraint(): CPOutcome = return cstr.updateMax(x,v)
}

class PropagEventUpdateMaxIdx(cstr: Constraint, x: CPVarInt, idx: Int, v: Int) extends PropagEvent(cstr,cstr.getPriorityBoundsL1()) {
	def notifyConstraint(): CPOutcome = return cstr.updateMaxIdx(x,idx,v)
}

class PropagEventUpdateMin(cstr: Constraint, x: CPVarInt, v: Int) extends PropagEvent(cstr,cstr.getPriorityBoundsL1()) {
	def notifyConstraint(): CPOutcome = return cstr.updateMin(x,v)
}

class PropagEventUpdateMinIdx(cstr: Constraint, x: CPVarInt, idx: Int, v: Int) extends PropagEvent(cstr,cstr.getPriorityBoundsL1()) {
	def notifyConstraint(): CPOutcome = return cstr.updateMinIdx(x,idx,v)
}

class PropagEventRequiredValue(cstr: Constraint, x: CPVarSet , idx: Int, value: Int) extends PropagEvent(cstr,cstr.getPriorityRequireL1()) {
	def notifyConstraint(): CPOutcome = return cstr.valRequiredIdx(x,idx,value);
}

class PropagEventExcludedValue(cstr: Constraint, x: CPVarSet , idx: Int, value: Int) extends PropagEvent(cstr,cstr.getPriorityExcludeL1()) {	
	def notifyConstraint(): CPOutcome = return cstr.valExcludedIdx(x,idx,value);
}
