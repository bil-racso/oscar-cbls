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

package oscar.cp.core;

/**
 * Represents a view on variable applying an offset on it.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class CPVarIntView extends CPVarInt {

	private CPVarInt v;
	private int b;
	
	/**
	 * Constructor for a view Y that represents v+b
	 */
	public CPVarIntView(CPVarInt v, int b) {
		super(v.getStore());
		this.v = v;
		this.b = b;
	}
	
	@Override
	public Store getStore() {
		return v.getStore();
	}
	
	@Override
	public boolean isBound() {
		return v.isBound();
	}
	
	@Override
	public int getSize() {
		return v.getSize();
	}
	
	@Override
	public boolean isEmpty() {
		return v.isEmpty();
	}	
	
	@Override
	public boolean hasValue(int val) {
		return v.hasValue(val-b);
	}
	
	@Override
	public CPOutcome updateMin(int val) {		
		return v.updateMin(val-b);
	}
	
	@Override
	public CPOutcome assign(int val) {
		return v.assign(val-b);
	}

	@Override
	public CPOutcome updateMax(int val) {
		return v.updateMax(val-b);
	}
	
	@Override
	public CPOutcome removeValue(int val) {
		return v.removeValue(val-b);
	}
	
	@Override
	public int getMin() {
		return v.getMin()+b;
	}
	
	@Override
	public int getMax() {
		return v.getMax()+b;
	}
	
	@Override
	public int getValue() {
		return v.getValue()+b;
	}
	
	@Override
	public String toString() {
		return "view with shift "+b+" on ("+v.toString()+")";
	}
	
	@Override
	public void callPropagateWhenBind(Constraint c) {
		v.callPropagateWhenBind(c);
	}
	
	@Override
	public void callPropagateWhenBoundsChange(Constraint c) {
		v.callPropagateWhenBoundsChange(c);
	}
	
	@Override
	public void callPropagateWhenMaxChanges(Constraint c) {
		v.callPropagateWhenMaxChanges(c);
	}
	
	@Override
	public void callPropagateWhenMinChanges(Constraint c) {
		v.callPropagateWhenMinChanges(c);
	}
	
	
	@Override
	public void callPropagateWhenDomainChanges(Constraint c) {
		v.callPropagateWhenDomainChanges(c);
	}
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callValBindWhenBind(Constraint c, CPVarInt var, int delta) {
		v.callValBindWhenBind(c, var, b+delta);
	}
	
	@Override
	public void callValBindWhenBind(Constraint c) {
		v.callValBindWhenBind(c,this,b);
	}
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callUpdateBoundsWhenBoundsChange(Constraint c, CPVarInt var, int delta) {
		v.callUpdateBoundsWhenBoundsChange(c, var, b+delta);
	}
	
	@Override
	public void callUpdateBoundsWhenBoundsChange(Constraint c) {
		v.callUpdateBoundsWhenBoundsChange(c,this,b);
	}
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callUpdateMaxWhenMaxChanges(Constraint c, CPVarInt var, int delta) {
		v.callUpdateMaxWhenMaxChanges(c,var,b+delta);
	}
	
	@Override
	public void callUpdateMaxWhenMaxChanges(Constraint c) {
		v.callUpdateMaxWhenMaxChanges(c,this,b);
	}
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callUpdateMinWhenMinChanges(Constraint c, CPVarInt var, int delta) {
		v.callUpdateMinWhenMinChanges(c,var,b+delta);
	}
	
	@Override
	public void callUpdateMinWhenMinChanges(Constraint c) {
		v.callUpdateMinWhenMinChanges(c,this,b);
	}
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callValRemoveWhenValueIsRemoved(Constraint c, CPVarInt var, int delta) {
		v.callValRemoveWhenValueIsRemoved(c,var,b+delta);
	}
		
	@Override
	public void callValRemoveWhenValueIsRemoved(Constraint c) {
		v.callValRemoveWhenValueIsRemoved(c,this,b);
	}
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callValBindIdxWhenBind(Constraint c, CPVarInt var,int idx, int delta) {
		v.callValBindIdxWhenBind(c, var,idx, b+delta);
	}	
	
	
	@Override
	public void callValBindIdxWhenBind(Constraint c, int idx) {
		v.callValBindIdxWhenBind(c,this,idx,b);
	}
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callUpdateBoundsIdxWhenBoundsChange(Constraint c, CPVarInt var, int idx, int delta) {
		v.callUpdateBoundsIdxWhenBoundsChange(c, var, idx, b+delta);
	}	
		
	@Override
	public void callUpdateBoundsIdxWhenBoundsChange(Constraint c, int idx) {
		v.callUpdateBoundsIdxWhenBoundsChange(c,this,idx,b);
	}
	
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callUpdateMaxIdxWhenMaxChanges(Constraint c, CPVarInt var, int idx, int delta) {
		v.callUpdateMaxIdxWhenMaxChanges(c,var,idx,b+delta);
	}	
	
	@Override
	public void callUpdateMaxIdxWhenMaxChanges(Constraint c, int idx) {
		v.callUpdateMaxIdxWhenMaxChanges(c,this,idx,b);
	}
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callUpdateMinIdxWhenMinChanges(Constraint c, CPVarInt var, int idx, int delta) {
		v.callUpdateMinIdxWhenMinChanges(c,var,idx,b+delta);
	}	
	
	@Override
	public void callUpdateMinIdxWhenMinChanges(Constraint c, int idx) {
		v.callUpdateMinIdxWhenMinChanges(c,this,idx,b);
	}
	
	// this method is useful when you have a view defined on a view
	@Override
	protected void callValRemoveIdxWhenValueIsRemoved(Constraint c, CPVarInt var, int idx, int delta) {
		v.callValRemoveIdxWhenValueIsRemoved(c,var,idx,b+delta);
	}	
	
	@Override
	public void callValRemoveIdxWhenValueIsRemoved(Constraint c, int idx) {
		v.callValRemoveIdxWhenValueIsRemoved(c,this,idx,b);
	}

}
