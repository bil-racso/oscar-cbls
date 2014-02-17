/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.cp.core

import oscar.cp.constraints.InSet
import oscar.cp.constraints.InSetReif
import oscar.cp.constraints.ModuloLHS

trait DomainIterator extends Iterator[Int] {
  def removeValue: CPOutcome
  def execute()
}

abstract class CPIntVar(override val store: CPStore, override val name: String = "") extends CPVar with Iterable[Int] {

  def rootVar: CPIntVar

  def offset: Int

  def constraintDegree(): Int

  /**
   * @return difference between second smallest and smallest value in the domain, Int.MaxInt if variable is bound
   */
  def regret: Int = if (isBound) Int.MaxValue else valueAfter(min) - min

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  def isBound: Boolean

  /**
   *
   * @param v
   * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
   */
  def isBoundTo(v: Int): Boolean

  /**
   * Test if a value is in the domain
   * @param val
   * @return  true if the domain contains the value val, false otherwise
   */
  def hasValue(value: Int): Boolean

  /**
   * @return the unique value in the domain, None if variable is not bound
   */
  def value: Int = min

  def getValue: Int = min

  /**
   * @param val
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  def valueAfter(value: Int): Int

  /**
   * @param val
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  def valueBefore(value: Int): Int

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  def randomValue: Int = {
    val ind = store.getRandom().nextInt(size);
    this.toArray.apply(ind)
    /*
		var cpt = 0;
		for (v: Int <- this) {
			if (cpt == ind) {
				return v
			}
			cpt += 1
		}
		min
		*/
  }

  /**
   * @return The median value of the domain of the variable
   */
  def median: Int = {

    val vals = this.toArray.sortBy(i => i)
    return vals(vals.size / 2)
  }

  /**
   * @return  the size of the domain
   */
  def size: Int

  def getSize = size

  /**
   * @return true is the domain is full
   */
  def isFull = (max - min + 1) == size

  /**
   * Number of values in common in both domains
   * @param other
   * @return Number of values in common in both domains
   */
  def intersectionSize(other: CPIntVar): Int = {
    if (other.min > max) return 0
    if (other.max < min) return 0
    var res = 0
    var v = other.min.max(min)
    while (v <= other.max.min(max)) {
      if (hasValue(v) && other.hasValue(v)) {
        res += 1
      }
      v += 1
    }
    res
  }

  /**
   * @return true if the domain is empty, false otherwise
   */
  def isEmpty: Boolean

  /**
   * @return  the minimum value in the domain
   */
  def min: Int

  def getMin = min

  /**
   * @return  the maximum value in the domain
   */
  def max: Int

  def getMax = max

  def domainIterator: DomainIterator = {
    new DomainIterator {
      private val it = iterator
      private var ok = false
      private var v = Int.MinValue
      private var collect: List[Int] = Nil
      private var maxRemove = CPIntVar.this.size

      def next(): Int = {
        v = it.next()
        ok = true
        v
      }
      def hasNext: Boolean = {
        it.hasNext
      }

      def removeValue(): CPOutcome = {
        assert(ok == true)
        ok = false
        collect = v :: collect
        maxRemove -= 1
        if (maxRemove <= 0)
          CPOutcome.Failure
        else
          CPOutcome.Suspend
      }

      def execute() = {
        for (v <- collect) CPIntVar.this.removeValue(v)
      }
    }
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBind(c: Constraint, trackDelta: Boolean = false): Unit

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the maximum or the minimum value of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBoundsChange(c: Constraint, trackDelta: Boolean = false): Unit

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * one of the value is removed from the domain
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false): Unit

  def filterWhenBind(filter: DeltaVarInt => CPOutcome) {
    store.post(
      new DeltaVarInt(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenBind(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  def filterWhenBoundsChanges(filter: DeltaVarInt => CPOutcome) {
    store.post(
      new DeltaVarInt(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenBoundsChange(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  def filterWhenDomainChanges(filter: DeltaVarInt => CPOutcome) {
    store.post(
      new DeltaVarInt(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenDomainChanges(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  /**
   * Level 1 registration: ask that the valBind(CPIntVar) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#valBind(CPIntVar)
   */
  def callValBindWhenBind(c: Constraint): Unit

  def callValBindWhenBind(c: Constraint, variable: CPIntVar, delta: Int): Unit

  /**
   * Level 1 registration: ask that the updateBounds(CPIntVar) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateBounds(CPIntVar)
   */
  def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit

  def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar, delta: Int): Unit

  /**
   * Level 1 registration: ask that the valRemove(CPIntVar, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @see oscar.cp.core.Constraint#valRemove(CPIntVar, int)
   */
  def callValRemoveWhenValueIsRemoved(c: Constraint): Unit

  def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar, delta: Int): Unit

  /**
   * Level 1 registration: ask that the valRemoveIdx(CPIntVar, int, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @param idx, an index that will be given as parameter to valRemoveIdx(CPIntVar, int, int)
   * @see Constraint#valRemoveIdx(CPIntVar, int, int)
   */
  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int): Unit

  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int, delta: Int): Unit


  /**
   * Level 1 registration: ask that the updateBoundsIdx(CPIntVar, int) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateBoundsIdx(CPIntVar, int)
   * @see Constraint#updateBoundsIdx(CPIntVar, int)
   */
  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit

  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int, delta: Int): Unit

  /**
   * Level 1 registration: ask that the valBindIdx(CPIntVar, int) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @param idx, an index that will be given as parameter to valBindIdx(CPIntVar, int)
   * @see Constraint#valBindIdx(CPIntVar, int)
   */
  def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit

  def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int, delta: Int): Unit

  /**
   * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if val was in the domain, Failure otherwise
   */
  def assign(value: Int): CPOutcome

  /**
   * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
   */
  def updateMin(value: Int): CPOutcome

  /**
   * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
   */
  def updateMax(value: Int): CPOutcome

  /**
   * Remove val from the domain, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if the domain is not equal to the singleton {val}, Failure otherwise
   */
  def removeValue(value: Int): CPOutcome

  // ------ delta methods to be called in propagate -------

  def changed(sn: SnapshotVarInt): Boolean = {
    sn.oldSize != size
  }

  def minChanged(sn: SnapshotVarInt): Boolean = {
    assert(sn.oldMin <= min)
    sn.oldMin < min
  }

  def maxChanged(sn: SnapshotVarInt): Boolean = {
    assert(sn.oldMax >= max)
    sn.oldMax > max
  }

  def boundsChanged(sn: SnapshotVarInt): Boolean = {
    sn.oldMax == max
  }

  def oldMin(sn: SnapshotVarInt): Int = {
    assert(sn.oldMin <= min)
    sn.oldMin
  }

  def oldMax(sn: SnapshotVarInt): Int = {
    assert(sn.oldMax >= max)
    sn.oldMax
  }

  def oldSize(sn: SnapshotVarInt): Int = {
    assert(sn.oldSize >= size)
    sn.oldSize
  }

  def deltaSize(sn: SnapshotVarInt): Int = {
    sn.oldSize - size
  }

  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int]

  // --------------------------------------------

  def changed(c: Constraint): Boolean

  def minChanged(c: Constraint): Boolean

  def maxChanged(c: Constraint): Boolean

  def boundsChanged(c: Constraint): Boolean

  def oldMin(c: Constraint): Int

  def oldMax(c: Constraint): Int

  def oldSize(c: Constraint): Int

  def deltaSize(c: Constraint): Int

  def delta(c: Constraint): Iterator[Int]

  // ------------------------ some useful methods for java -------------------------

  /**
   * @return a variable in the same store representing: - x
   */
  def opposite() = {
    val y = new CPIntVarImpl(store, -max, -min);
    store.post(new oscar.cp.constraints.Opposite(this, y));
    y;
  }

  /**
   * @param d
   * @return  a variable in the same store representing: x - d
   */
  def minus(d: Int) = {
    if (d == 0) this
    else new CPIntVarView(this, -d)
  }

  /**
   * @param y a variable in the same store as x
   * @return a variable in the same store representing: x - y
   */
  def minus(y: CPIntVar) = {
    val c = new CPIntVarImpl(store, min - y.max, max - y.min);
    store.post(new oscar.cp.constraints.Minus(this, y, c));
    c;
  }

  /**
   * @param d
   * @return  a variable in the same store representing: x + d
   */
  def plus(d: Int) = {
    if (d == 0) this;
    else new CPIntVarView(this, d);
  }

  /**
   * @param y
   * @return a variable in the same store representing: x + y
   */
  def plus(y: CPIntVar): CPIntVar = {
    if (y.isBound) {
      this.plus(y.value)
    } else {
      val c = new CPIntVarImpl(store, min + y.min, max + y.max);
      val ok = store.post(new oscar.cp.constraints.BinarySum(this, y, c));
      assert(ok != CPOutcome.Failure);
      c
    }
  }

  /**
   * @param c
   * @return a variable in the same store representing: x * c
   */
  def mul(c: Int): CPIntVar = {
    if (c == 1) {
      return this
    }
    val a = if (c > 0) min * c else max * c
    val b = if (c > 0) max * c else min * c
    val y = new CPIntVarImpl(store, a, b)
    val ok = store.post(new oscar.cp.constraints.MulCte(this, c, y))
    assert(ok != CPOutcome.Failure)
    return y;
  }

  /**
   * @param y a variable in the same store as x
   * @return a variable in the same store representing: x * y
   */
  def mul(y: CPIntVar): CPIntVar = {
    val a = min
    val b = max
    val c = y.min
    val d = y.max
    import oscar.cp.util.NumberUtils
    val t = Array(NumberUtils.safeMul(a, c), NumberUtils.safeMul(a, d), NumberUtils.safeMul(b, c), NumberUtils.safeMul(b, d));
    val z = new CPIntVarImpl(store, t.min, t.max)
    val ok = store.post(new oscar.cp.constraints.MulVar(this, y, z))
    assert(ok != CPOutcome.Failure);
    z
  }

  /**
   * @return a variable in the same store representing: |x|
   */
  def abs(): CPIntVar = {
    val c = new CPIntVarImpl(store, 0, Math.max(Math.abs(min), Math.abs(max)));
    val ok = store.post(new oscar.cp.constraints.Abs(this, c));
    assert(ok != CPOutcome.Failure);
    return c
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x == v <=> b == true
   */
  def isEq(v: Int): CPBoolVar = {
    val b = new CPBoolVar(store);
    val ok = store.post(new oscar.cp.constraints.EqReif(this, v, b));
    assert(ok != CPOutcome.Failure);
    return b;
  }

  /**
   * Reified constraint
   * @param y a variable
   * @return a boolean variable b in the same store linked to x by the relation x == y <=> b == true
   */
  def isEq(y: CPIntVar): CPBoolVar = {
    val b = new CPBoolVar(store);
    val ok = store.post(new oscar.cp.constraints.EqReifVar(this, y, b));
    assert(ok != CPOutcome.Failure);
    b
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x != v <=> b == true
   */
  def isDiff(v: Int): CPBoolVar = {
    val b = new CPBoolVar(store);
    val ok = store.post(new oscar.cp.constraints.DiffReif(this, v, b));
    assert(ok != CPOutcome.Failure)
    return b;
  }

  /**
   * Reified constraint
   * @param y
   * @return  a boolean variable b in the same store linked to x by the relation x != y <=> b == true
   */
  def isDiff(y: CPIntVar): CPBoolVar = {
    val b = new CPBoolVar(store);
    val ok = store.post(new oscar.cp.constraints.DiffReifVar(this, y, b));
    assert(ok != CPOutcome.Failure)
    return b;
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x >= v <=> b == true
   */
  def isGrEq(v: Int): CPBoolVar = {
    val b = new CPBoolVar(store);
    val ok = store.post(new oscar.cp.constraints.GrEqCteReif(this, v, b));
    assert(ok != CPOutcome.Failure);
    return b;
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x <= v <=> b == true
   */
  def isLeEq(v: Int): CPBoolVar = {
    val b = new CPBoolVar(store);
    val ok = store.post(new oscar.cp.constraints.LeEqCteReif(this, v, b));
    assert(ok != CPOutcome.Failure);
    return b;
  }

  /**
   * Reified constraint
   * @param y a variable in the same store as x
   * @return  a boolean variable b in the same store linked to x by the relation x >= y <=> b == true
   */
  def isGrEq(y: CPIntVar): CPBoolVar = {
    val b = new CPBoolVar(store);
    val ok = store.post(new oscar.cp.constraints.GrEqVarReif(this, y, b));
    assert(ok != CPOutcome.Failure);
    return b;
  }

  def isRange: Boolean = (max - min + 1) == size

  /**
   * x must take a value from set
   */
  def in(set: Set[Int]): Constraint = new InSet(this, set)

  /**
   * -x
   */
  def unary_-() = this.opposite()
  /**
   * x+y
   */
  def +(y: CPIntVar) = this.plus(y)
  /**
   * x-y
   */
  def -(y: CPIntVar) = this.minus(y)
  /**
   * x+y
   */
  def +(y: Int) = this.plus(y)
  /**
   * x-y
   */
  def -(y: Int) = this.minus(y)
  /**
   * x*y
   */
  def *(y: CPIntVar) = this.mul(y)
  /**
   * x*y
   */
  def *(y: Int) = this.mul(y)
  /**
   * x!=y
   */
  def !=(y: CPIntVar) = new oscar.cp.constraints.DiffVar(this, y)
  /**
   * x!=y
   */
  def !=(y: Int) = new oscar.cp.constraints.DiffVal(this, y)
  /**
   * x==y
   */
  def ==(y: CPIntVar) = new oscar.cp.constraints.Eq(this, y)
  /**
   * x==y
   */
  def ==(y: Int) = new oscar.cp.constraints.EqVal(this, y)
  /**
   * x<y
   */
  def <(y: CPIntVar) = new oscar.cp.constraints.Le(this, y)
  /**
   * x<y
   */
  def <(y: Int) = new oscar.cp.constraints.Le(this, y)
  /**
   * x>y
   */
  def >(y: CPIntVar) = new oscar.cp.constraints.Gr(this, y)
  /**
   * x>y
   */
  def >(y: Int) = new oscar.cp.constraints.Gr(this, y)
  /**
   * x<=y
   */
  def <=(y: CPIntVar) = new oscar.cp.constraints.LeEq(this, y)
  /**
   * x<=y
   */
  def <=(y: Int) = new oscar.cp.constraints.LeEq(this, y)
  /**
   * x>=y
   */
  def >=(y: CPIntVar) = new oscar.cp.constraints.GrEq(this, y)
  /**
   * x>=y
   */
  def >=(y: Int) = new oscar.cp.constraints.GrEq(this, y)
  /**
   * b <=> x == v
   */
  def ===(v: Int) = this.isEq(v)
  /**
   * b <=> x == y
   */
  def ===(y: CPIntVar) = this.isEq(y)
  /**
   * b <=> x!= y
   */
  def !==(y: CPIntVar) = this.isDiff(y)
  /**
   * b <=> x!= y
   */
  def !==(y: Int) = this.isDiff(y)
  /**
   * b <=> x >= y
   */
  def >==(y: Int) = this.isGrEq(y)
  /**
   * b <=> x >= y
   */
  def >==(y: CPIntVar) = this.isGrEq(y)
  /**
   * b <=> x > y
   */
  def >>=(y: Int) = this.isGrEq(y + 1)
  /**
   * b <=> x > y
   */
  def >>=(y: CPIntVar) = this.isGrEq(y + 1)
  /**
   * b <=> x >= y
   */
  def <==(y: Int) = this.isLeEq(y)
  /**
   * b <=> x >= y
   */
  def <==(y: CPIntVar) = y >== this
  /**
   * b <=> x > y
   */
  def <<=(y: Int) = this <== (y - 1)
  /**
   * b <=> x > y
   */
  def <<=(y: CPIntVar) = this <== (y - 1)

  /**
   * b <=> x belongs to set
   */
  def isIn(set: Set[Int]): CPBoolVar = {
    val b = CPBoolVar()(store)
    store.post(new InSetReif(this, set, b))
    b
  }
  
  def %(y: Int) = ModuloLHS(this, y)

}

object CPIntVar {

  /**
   * Creates a new CP Integer Variable with an iterable as initial domain
   * @param values the iterable defining the possible values for the variable
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Iterable[Int], name: String)(implicit store: CPStore): CPIntVar = {
    values match {
      case range: Range => rangeDomain(range, name, store)
      case set: Set[Int] => setDomain(set, name, store)
      case iterable => iterableDomain(iterable, name, store)
    }
  }

  /**
   * Creates a new CP Integer Variable with an iterable as initial domain
   * @param values the iterable defining the possible values for the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Iterable[Int])(implicit store: CPStore): CPIntVar = apply(values, "")(store)
  
  /**
   * Creates a new CP Integer Variable with an array as initial domain
   * @param values the array defining the possible values for the variable
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Array[Int], name: String)(implicit store: CPStore): CPIntVar = {
    iterableDomain(values, name, store)
  }
  
  /**
   * Creates a new CP Integer Variable with an array as initial domain
   * @param values the array defining the possible values for the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Array[Int])(implicit store: CPStore): CPIntVar = apply(values, "")(store)

  /**
   * Creates a new CP Integer Variable with all the values contained in (minValue to maxValue) as initial domain
   * @param minValue the minimal value of the domain
   * @param maxValue the maximal value of the domain
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with all the values contained in (minValue to maxValue)
   * as initial domain.
   */
  def apply(minValue: Int, maxValue: Int, name: String)(implicit store: CPStore): CPIntVar = {
    new CPIntVarImpl(store, minValue, maxValue, name)
  }

  /**
   * Creates a new CP Integer Variable with all the values contained in (minValue to maxValue) as initial domain
   * @param minValue the minimal value of the domain
   * @param maxValue the maximal value of the domain
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with all the values contained in (minValue to maxValue)
   * as initial domain.
   */
  def apply(minValue: Int, maxValue: Int)(implicit store: CPStore): CPIntVar = apply(minValue, maxValue, "")(store)

  /**
   * Creates a new CP Integer Variable assigned to value
   * @param value the single value contained in the domain
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with a single value as initial domain.
   */
  def apply(value: Int, name: String)(implicit store: CPStore): CPIntVar = new CPIntVarImpl(store, value, value, name)

  /**
   * Creates a new CP Integer Variable assigned to value
   * @param value the single value contained in the domain
   * @param store the CPStore in which the variable is created
   * @return a fresh CPIntVar defined in the CPStore store with a single value as initial domain.
   */
  def apply(value: Int)(implicit store: CPStore): CPIntVar = new CPIntVarImpl(store, value, value, "")

  @deprecated("use apply(values: Iterable[Int], name: String)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, values: Iterable[Int], name: String): CPIntVar = apply(values, name)(store)

  @deprecated("use apply(values: Iterable[Int])(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, values: Iterable[Int]): CPIntVar = apply(store, values, "")
  
  @deprecated("use apply(values: Array[Int], name: String)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, values: Array[Int], name: String): CPIntVar = apply(values, name)(store)

  @deprecated("use apply(values: Array[Int])(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, values: Array[Int]): CPIntVar = apply(store, values, "")

  @deprecated("use apply(minValue: Int, maxValue: Int, name: String)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, minValue: Int, maxValue: Int, name: String): CPIntVar = apply(minValue, maxValue, name)(store)

  @deprecated("use apply(minValue: Int, maxValue: Int)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, minValue: Int, maxValue: Int): CPIntVar = apply(minValue, maxValue, "")(store)

  @deprecated("use apply(value: Int, name: String)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, value: Int, name: String): CPIntVar = apply(value, name)(store)

  @deprecated("use apply(value: Int)(implicit store: CPStore) instead", "1.0")
  def apply(store: CPStore, value: Int): CPIntVar = apply(value, "")(store)

  /** Builds a CPIntVar from a range */
  private def rangeDomain(domain: Range, name: String, store: CPStore): CPIntVar = {
    if (domain.max - domain.min < domain.size - 1) iterableDomain(domain, name, store)
    else new CPIntVarImpl(store, domain.min, domain.max, name)
  }

  /** Builds a CPIntVar from an iterable */
  private def iterableDomain(domain: Iterable[Int], name: String, store: CPStore): CPIntVar = setDomain(domain.toSet, name, store)

  /** Builds a CPIntVar from a set */
  private def setDomain(domain: Set[Int], name: String, store: CPStore): CPIntVar = {
    val min = domain.min
    val max = domain.max
    val x = new CPIntVarImpl(store, min, max, name)
    if (max - min + 1 > domain.size) {
      for (v <- min to max if !domain.contains(v)) {
        x.removeValue(v)
      }
    }
    x
  }
}

  
