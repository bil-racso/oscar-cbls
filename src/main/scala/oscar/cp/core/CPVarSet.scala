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

import oscar.reversible.ReversibleQueue
import oscar.reversible.ReversiblePointer
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints.Requires
import oscar.cp.constraints.Excludes
import oscar.cp.constraints.SetCard

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPVarSet(val s: CPStore, min: Int, max: Int, val name: String = "") extends CPVar {

  def store = s
  private val dom = new SetDomain(s, min, max);

  val onDomainL2 = new ReversiblePointer[ConstraintQueue](s, null)

  val onRequiredL1 = new ReversiblePointer[PropagEventQueueVarSet](s, null)
  val onExcludedL1 = new ReversiblePointer[PropagEventQueueVarSet](s, null)
  val onRequiredIdxL1 = new ReversiblePointer[PropagEventQueueVarSet](s, null)
  val onExcludedIdxL1 = new ReversiblePointer[PropagEventQueueVarSet](s, null)

  // cardinality variable
  val card = CPVarInt(s,0,max-min+1);
  s.post(new SetCard(this,card));

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  def isBound: Boolean = dom.possibleSize == dom.requiredSize

  /**
   * Test if a value is in the possible values
   * @param val
   * @return  true if value is in the possible values false otherwise
   */
  def isPossible(value: Int) = dom.isPossible(value)

  /**
   * Test if a value is in the required values
   * @param val
   * @return  true if value is in the required values false otherwise
   */
  def isRequired(value: Int) = dom.isRequired(value)

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever the domain of the variable changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) {
    onDomainL2.setValue(new ConstraintQueue(onDomainL2.value, c));
    if (trackDelta) c.addSnapshot(this)
  }

  def filterWhenDomainChanges(filter: DeltaVarSet => CPOutcome) {
    s.post(
      new DeltaVarSet(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenDomainChanges(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValRequiredWhenRequiredValue(c: Constraint) {
    onRequiredL1.setValue(new PropagEventQueueVarSet(onRequiredL1.value, c, this))
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValExcludedWhenExcludedValue(c: Constraint) {
    onExcludedL1.setValue(new PropagEventQueueVarSet(onExcludedL1.value, c, this))
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValRequiredIdxWhenRequiredValue(c: Constraint, idx: Int) {
    onRequiredIdxL1.setValue(new PropagEventQueueVarSet(onRequiredIdxL1.value, c, this, idx))
  }

  /**
   * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callValExcludedIdxWhenExcludedValue(c: Constraint, idx: Int) {
    onExcludedIdxL1.setValue(new PropagEventQueueVarSet(onExcludedIdxL1.value, c, this, idx))
  }

  def requires(v: Int): CPOutcome = {
    if (dom.isPossible(v) && !dom.isRequired(v)) {
      // -------- AC3 notifications ------------
      s.notifyL2(onDomainL2.value)
      // -------- AC5 notifications ------------
      s.notifyRequired(onRequiredL1.value, this, v)
      s.notifyRequiredIdx(onRequiredIdxL1.value, this, v)
    }
    val oc = dom.requires(v)
    if (oc != CPOutcome.Failure) {
      if (requiredSize == card.max) {
        for (a: Int <- possibleNotRequiredValues.toSet) {
          val r = excludes(a)
          assert(r != CPOutcome.Failure)
        }
      }
      card.updateMin(requiredSize)
    }
    else oc
  }

  def excludes(v: Int): CPOutcome = {
    if (dom.isPossible(v) && !dom.isRequired(v)) {
      // -------- AC3 notifications ------------
      s.notifyL2(onDomainL2.value)
      // -------- AC5 notifications ------------
      s.notifyExcluded(onExcludedL1.value, this, v)
      s.notifyExcludedIdx(onExcludedIdxL1.value, this, v)
    }
    val oc = dom.excludes(v)
    if (oc != CPOutcome.Failure) {
      if (possibleSize == card.min) {
        for (a: Int <- possibleNotRequiredValues.toSet) {
          val r = requires(a)
           assert(r != CPOutcome.Failure)
        }
      }
      card.updateMax(possibleSize)
    }    
    else oc
  }

  def requiresAll(): CPOutcome = {
    // -------- AC3 notifications ------------
    if (possibleSize > requiredSize) s.notifyL2(onDomainL2.value)
    // -------- AC5 notifications ------------
    if (onRequiredL1.hasValue() || onRequiredIdxL1.hasValue) {
      for (v <- dom.possibleNotRequiredValues) {
        if (onRequiredL1.hasValue()) s.notifyRequired(onRequiredL1.value, this, v)
        if (onRequiredIdxL1.hasValue) s.notifyRequiredIdx(onRequiredIdxL1.value, this, v)
      }
    }
    val oc = dom.requiresAll()
    assert(oc != CPOutcome.Failure)
    card.assign(requiredSize)
  }

  def excludesAll(): CPOutcome = {
    // -------- AC3 notifications ------------
    if (possibleSize > requiredSize) s.notifyL2(onDomainL2.value)
    // -------- AC5 notifications ------------
    if (onExcludedL1.hasValue() || onExcludedIdxL1.hasValue) {
      for (v <- dom.possibleNotRequiredValues) {
        if (onExcludedL1.hasValue()) s.notifyExcluded(onExcludedL1.value, this, v)
        if (onExcludedIdxL1.hasValue) s.notifyExcludedIdx(onExcludedIdxL1.value, this, v)
      }
    }    
    val oc = dom.excludesAll()
    assert(oc != CPOutcome.Failure)
    card.assign(requiredSize)
  }
  
   
  def arbitraryPossibleNotRequired: Int = dom.arbitraryPossibleNotRequired
  
  def randomPossibleNotRequired: Int = dom.randomPossibleNotRequired    

  def value(): Set[Int] = dom.requiredSet

  def requiredSet(): Set[Int] = dom.requiredSet

  def possibleSet(): Set[Int] = dom.possibleSet
  
  def possibleNotRequiredValues: Iterator[Int] = dom.possibleNotRequiredValues
   
  def requiredValues: Iterator[Int] = dom.requiredValues
  

  def possibleSize = dom.possibleSize

  def requiredSize = dom.requiredSize

  def include(v: Int) = new Requires(this, v)
  def exclude(v: Int) = new Excludes(this, v)
  
  override def toString = {
    ""+requiredValues.toSet+" "+possibleNotRequiredValues.toSet
  }

  // --------------------------------------------

  // ------ delta methods to be called in propagate -------

  def changed(sn: SnapshotVarSet) = possibleChanged(sn) || requiredChanged(sn)

  def possibleChanged(sn: SnapshotVarSet) = {
    sn.oldSizePossible != possibleSize
  }

  def requiredChanged(sn: SnapshotVarSet) = {
    sn.oldSizeRequired != requiredSize
  }

  def deltaPossibleSize(sn: SnapshotVarSet): Int = sn.oldSizePossible - possibleSize

  def deltaRequiredSize(sn: SnapshotVarSet): Int = requiredSize - sn.oldSizeRequired

  def deltaPossible(sn: SnapshotVarSet): Iterator[Int] = dom.deltaPossible(sn.oldSizePossible)

  def deltaRequired(sn: SnapshotVarSet): Iterator[Int] = dom.deltaRequired(sn.oldSizeRequired)

  // --------------------------------------------

}

object CPVarSet {
	def apply(s: CPStore,required: Set[Int] = Set(), possibleNotRequired: Set[Int]): CPVarSet = {
	  if (!required.intersect(possibleNotRequired).isEmpty) {
	    throw new RuntimeException("Possible values should not be required")
	  }
	  val allPossibles = required ++ possibleNotRequired
	  val x = new CPVarSet(s,allPossibles.min,allPossibles.max)
	  for (v <- allPossibles.min to allPossibles.max if !allPossibles.contains(v)) {
	    x.excludes(v)
	  }
	  for (v <- required) {
	    x.requires(v)
	  }
	  x
	}
}
  
