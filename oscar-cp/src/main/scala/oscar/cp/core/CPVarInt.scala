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

trait DomainIterator extends Iterator[Int] {
  def removeValue: CPOutcome
  def execute()
}

abstract class CPVarInt(val s: CPStore, val name: String = "") extends CPVar with Iterable[Int] {

  def store = s

  def rootVar: CPVarInt

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
    val ind = s.getRandom().nextInt(size);
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
  def intersectionSize(other: CPVarInt): Int = {
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
      private var maxRemove = CPVarInt.this.size

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
        for (v <- collect) CPVarInt.this.removeValue(v)
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
   * the maximum of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenMaxChanges(c: Constraint, trackDelta: Boolean = false): Unit

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the minimum of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenMinChanges(c: Constraint, trackDelta: Boolean = false): Unit

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * one of the value is removed from the domain
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false): Unit

  def filterWhenBind(filter: DeltaVarInt => CPOutcome) {
    s.post(
      new DeltaVarInt(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenBind(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  def filterWhenBoundsChanges(filter: DeltaVarInt => CPOutcome) {
    s.post(
      new DeltaVarInt(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenBoundsChange(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  def filterWhenMaxChanges(filter: DeltaVarInt => CPOutcome) {
    s.post(
      new DeltaVarInt(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenMaxChanges(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  def filterWhenMinChanges(filter: DeltaVarInt => CPOutcome) {
    s.post(
      new DeltaVarInt(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenMinChanges(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  def filterWhenDomainChanges(filter: DeltaVarInt => CPOutcome) {
    s.post(
      new DeltaVarInt(this, filter) {
        def setup(l: CPPropagStrength) = {
          callPropagateWhenDomainChanges(this)
          CPOutcome.Suspend
        }
      }) // should not fail
  }

  /**
   * Level 1 registration: ask that the valBind(CPVarInt) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#valBind(CPVarInt)
   */
  def callValBindWhenBind(c: Constraint): Unit

  def callValBindWhenBind(c: Constraint, variable: CPVarInt, delta: Int): Unit

  /**
   * Level 1 registration: ask that the updateBounds(CPVarInt) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateBounds(CPVarInt)
   */
  def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit

  def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPVarInt, delta: Int): Unit

  /**
   * Level 1 registration: ask that the updateMax(CPVarInt, int) method of the constraint c is called whenever
   * the maximum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateMax(CPVarInt, int)
   */
  def callUpdateMaxWhenMaxChanges(c: Constraint): Unit

  def callUpdateMaxWhenMaxChanges(c: Constraint, variable: CPVarInt, delta: Int): Unit

  /**
   * Level 1 registration: ask that the updateMin(CPVarInt, int) method of the constraint c is called whenever
   * the minimum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateMin(CPVarInt, int)
   */
  def callUpdateMinWhenMinChanges(c: Constraint): Unit

  def callUpdateMinWhenMinChanges(c: Constraint, variable: CPVarInt, delta: Int): Unit

  /**
   * Level 1 registration: ask that the valRemove(CPVarInt, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @see oscar.cp.core.Constraint#valRemove(CPVarInt, int)
   */
  def callValRemoveWhenValueIsRemoved(c: Constraint): Unit

  def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPVarInt, delta: Int): Unit

  /**
   * Level 1 registration: ask that the valRemoveIdx(CPVarInt, int, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @param idx, an index that will be given as parameter to valRemoveIdx(CPVarInt, int, int)
   * @see Constraint#valRemoveIdx(CPVarInt, int, int)
   */
  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int): Unit

  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit

  /**
   * Level 1 registration: ask that the updateMinIdx(CPVarInt, int, int) method of the constraint c is called whenever
   * the minimum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateMinIdx(CPVarInt, int, int)
   * @see Constraint#updateMinIdx(CPVarInt, int, int)
   */
  def callUpdateMinIdxWhenMinChanges(c: Constraint, idx: Int): Unit

  def callUpdateMinIdxWhenMinChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit

  /**
   * Level 1 registration: ask that the updateMaxIdx(CPVarInt, int, int) method of the constraint c is called whenever
   * the maximum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateMaxIdx(CPVarInt, int, int)
   * @see Constraint#updateMaxIdx(CPVarInt, int, int)
   */
  def callUpdateMaxIdxWhenMaxChanges(c: Constraint, idx: Int): Unit

  def callUpdateMaxIdxWhenMaxChanges(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit

  /**
   * Level 1 registration: ask that the updateBoundsIdx(CPVarInt, int) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateBoundsIdx(CPVarInt, int)
   * @see Constraint#updateBoundsIdx(CPVarInt, int)
   */
  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit

  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit

  /**
   * Level 1 registration: ask that the valBindIdx(CPVarInt, int) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @param idx, an index that will be given as parameter to valBindIdx(CPVarInt, int)
   * @see Constraint#valBindIdx(CPVarInt, int)
   */
  def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit

  def callValBindIdxWhenBind(c: Constraint, variable: CPVarInt, idx: Int, delta: Int): Unit

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
    val y = new CPVarIntImpl(s, -max, -min);
    s.post(new oscar.cp.constraints.Opposite(this, y));
    y;
  }

  /**
   * @param d
   * @return  a variable in the same store representing: x - d
   */
  def minus(d: Int) = {
    if (d == 0) this
    else new CPVarIntView(this, -d)
  }

  /**
   * @param y a variable in the same store as x
   * @return a variable in the same store representing: x - y
   */
  def minus(y: CPVarInt) = {
    val c = new CPVarIntImpl(s, min - y.max, max - y.min);
    s.post(new oscar.cp.constraints.Minus(this, y, c));
    c;
  }

  /**
   * @param d
   * @return  a variable in the same store representing: x + d
   */
  def plus(d: Int) = {
    if (d == 0) this;
    else new CPVarIntView(this, d);
  }

  /**
   * @param y
   * @return a variable in the same store representing: x + y
   */
  def plus(y: CPVarInt): CPVarInt = {
    if (y.isBound) {
      this.plus(y.value)
    } else {
      val c = new CPVarIntImpl(s, min + y.min, max + y.max);
      val ok = s.post(new oscar.cp.constraints.BinarySum(this, y, c));
      assert(ok != CPOutcome.Failure);
      c
    }
  }

  /**
   * @param c
   * @return a variable in the same store representing: x * c
   */
  def mul(c: Int): CPVarInt = {
    if (c == 1) {
      return this
    }
    val a = if (c > 0) min * c else max * c
    val b = if (c > 0) max * c else min * c
    val y = new CPVarIntImpl(s, a, b)
    val ok = s.post(new oscar.cp.constraints.MulCte(this, c, y))
    assert(ok != CPOutcome.Failure)
    return y;
  }

  /**
   * @param y a variable in the same store as x
   * @return a variable in the same store representing: x * y
   */
  def mul(y: CPVarInt): CPVarInt = {
    val a = min
    val b = max
    val c = y.min
    val d = y.max
    import oscar.cp.util.NumberUtils
    val t = Array(NumberUtils.safeMul(a, c), NumberUtils.safeMul(a, d), NumberUtils.safeMul(b, c), NumberUtils.safeMul(b, d));
    val z = new CPVarIntImpl(s, t.min, t.max)
    val ok = s.post(new oscar.cp.constraints.MulVar(this, y, z))
    assert(ok != CPOutcome.Failure);
    z
  }

  /**
   * @return a variable in the same store representing: |x|
   */
  def abs(): CPVarInt = {
    val c = new CPVarIntImpl(s, 0, Math.max(Math.abs(min), Math.abs(max)));
    val ok = s.post(new oscar.cp.constraints.Abs(this, c));
    assert(ok != CPOutcome.Failure);
    return c
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x == v <=> b == true
   */
  def isEq(v: Int): CPVarBool = {
    val b = new CPVarBool(s);
    val ok = s.post(new oscar.cp.constraints.EqReif(this, v, b));
    assert(ok != CPOutcome.Failure);
    return b;
  }

  /**
   * Reified constraint
   * @param y a variable
   * @return a boolean variable b in the same store linked to x by the relation x == y <=> b == true
   */
  def isEq(y: CPVarInt): CPVarBool = {
    val b = new CPVarBool(s);
    val ok = s.post(new oscar.cp.constraints.EqReifVar(this, y, b));
    assert(ok != CPOutcome.Failure);
    b
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x != v <=> b == true
   */
  def isDiff(v: Int): CPVarBool = {
    val b = new CPVarBool(s);
    val ok = s.post(new oscar.cp.constraints.DiffReif(this, v, b));
    assert(ok != CPOutcome.Failure)
    return b;
  }

  /**
   * Reified constraint
   * @param y
   * @return  a boolean variable b in the same store linked to x by the relation x != y <=> b == true
   */
  def isDiff(y: CPVarInt): CPVarBool = {
    val b = new CPVarBool(s);
    val ok = s.post(new oscar.cp.constraints.DiffReifVar(this, y, b));
    assert(ok != CPOutcome.Failure)
    return b;
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x >= v <=> b == true
   */
  def isGrEq(v: Int): CPVarBool = {
    val b = new CPVarBool(s);
    val ok = s.post(new oscar.cp.constraints.GrEqCteReif(this, v, b));
    assert(ok != CPOutcome.Failure);
    return b;
  }

  /**
   * Reified constraint
   * @param v
   * @return  a boolean variable b in the same store linked to x by the relation x <= v <=> b == true
   */
  def isLeEq(v: Int): CPVarBool = {
    val b = new CPVarBool(s);
    val ok = s.post(new oscar.cp.constraints.LeEqCteReif(this, v, b));
    assert(ok != CPOutcome.Failure);
    return b;
  }

  /**
   * Reified constraint
   * @param y a variable in the same store as x
   * @return  a boolean variable b in the same store linked to x by the relation x >= y <=> b == true
   */
  def isGrEq(y: CPVarInt): CPVarBool = {
    val b = new CPVarBool(s);
    val ok = s.post(new oscar.cp.constraints.GrEqVarReif(this, y, b));
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
  def +(y: CPVarInt) = this.plus(y)
  /**
   * x-y
   */
  def -(y: CPVarInt) = this.minus(y)
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
  def *(y: CPVarInt) = this.mul(y)
  /**
   * x*y
   */
  def *(y: Int) = this.mul(y)
  /**
   * x!=y
   */
  def !=(y: CPVarInt) = new oscar.cp.constraints.DiffVar(this, y)
  /**
   * x!=y
   */
  def !=(y: Int) = new oscar.cp.constraints.DiffVal(this, y)
  /**
   * x==y
   */
  def ==(y: CPVarInt) = new oscar.cp.constraints.Eq(this, y)
  /**
   * x==y
   */
  def ==(y: Int) = new oscar.cp.constraints.EqVal(this, y)
  /**
   * x<y
   */
  def <(y: CPVarInt) = new oscar.cp.constraints.Le(this, y)
  /**
   * x<y
   */
  def <(y: Int) = new oscar.cp.constraints.Le(this, y)
  /**
   * x>y
   */
  def >(y: CPVarInt) = new oscar.cp.constraints.Gr(this, y)
  /**
   * x>y
   */
  def >(y: Int) = new oscar.cp.constraints.Gr(this, y)
  /**
   * x<=y
   */
  def <=(y: CPVarInt) = new oscar.cp.constraints.LeEq(this, y)
  /**
   * x<=y
   */
  def <=(y: Int) = new oscar.cp.constraints.LeEq(this, y)
  /**
   * x>=y
   */
  def >=(y: CPVarInt) = new oscar.cp.constraints.GrEq(this, y)
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
  def ===(y: CPVarInt) = this.isEq(y)
  /**
   * b <=> x!= y
   */
  def !==(y: CPVarInt) = this.isDiff(y)
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
  def >==(y: CPVarInt) = this.isGrEq(y)
  /**
   * b <=> x > y
   */
  def >>=(y: Int) = this.isGrEq(y + 1)
  /**
   * b <=> x > y
   */
  def >>=(y: CPVarInt) = this.isGrEq(y + 1)
  /**
   * b <=> x >= y
   */
  def <==(y: Int) = this.isLeEq(y)
  /**
   * b <=> x >= y
   */
  def <==(y: CPVarInt) = y >== this
  /**
   * b <=> x > y
   */
  def <<=(y: Int) = this <== (y - 1)
  /**
   * b <=> x > y
   */
  def <<=(y: CPVarInt) = this <== (y - 1)

  /**
   * b <=> x belongs to set
   */
  def isIn(set: Set[Int]): CPVarBool = {
    val b = CPVarBool(s)
    s.post(new InSetReif(this, set, b))
    b
  }

}

object CPVarInt {

  /**
   * Creates a new CP Integer Variable with an iterable as initial domain
   * @param values the iterable defining the possible values for the variable
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPVarInt defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Iterable[Int], name: String)(implicit store: CPStore): CPVarInt = {
    values match {
      case range: Range => rangeDomain(range, name, store)
      case set: Set[Int] => setDomain(set, name, store)
      case iterable => setDomain(iterable.toSet, name, store)
    }
  }

  /**
   * Creates a new CP Integer Variable with an iterable as initial domain
   * @param values the iterable defining the possible values for the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPVarInt defined in the CPStore store with values as initial domain.
   * The domain of the variable does not contains a given value more than once.
   */
  def apply(values: Iterable[Int])(implicit store: CPStore): CPVarInt = apply(values, "")(store)

  /**
   * Creates a new CP Integer Variable with all the values contained in (minValue to maxValue) as initial domain
   * @param minValue the minimal value of the domain
   * @param maxValue the maximal value of the domain
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPVarInt defined in the CPStore store with all the values contained in (minValue to maxValue)
   * as initial domain.
   */
  def apply(minValue: Int, maxValue: Int, name: String)(implicit store: CPStore): CPVarInt = {
    rangeDomain(minValue to maxValue, name, store)
  }

  /**
   * Creates a new CP Integer Variable with all the values contained in (minValue to maxValue) as initial domain
   * @param minValue the minimal value of the domain
   * @param maxValue the maximal value of the domain
   * @param store the CPStore in which the variable is created
   * @return a fresh CPVarInt defined in the CPStore store with all the values contained in (minValue to maxValue)
   * as initial domain.
   */
  def apply(minValue: Int, maxValue: Int)(implicit store: CPStore): CPVarInt = apply(minValue, maxValue, "")(store)

  /**
   * Creates a new CP Integer Variable assigned to value
   * @param value the single value contained in the domain
   * @param name the name of the variable
   * @param store the CPStore in which the variable is created
   * @return a fresh CPVarInt defined in the CPStore store with a single value as initial domain.
   */
  def apply(value: Int, name: String)(implicit store: CPStore): CPVarInt = new CPVarIntImpl(store, value, value, name)

  /**
   * Creates a new CP Integer Variable assigned to value
   * @param value the single value contained in the domain
   * @param store the CPStore in which the variable is created
   * @return a fresh CPVarInt defined in the CPStore store with a single value as initial domain.
   */
  def apply(value: Int)(implicit store: CPStore): CPVarInt = new CPVarIntImpl(store, value, value, "")

  @deprecated("Use apply(values: Iterable[Int], name: String)(implicit store: CPStore) instead", "version alpha")
  def apply(store: CPStore, values: Iterable[Int], name: String): CPVarInt = apply(values, name)(store)

  @deprecated("Use apply(values: Iterable[Int])(implicit store: CPStore) instead", "version alpha")
  def apply(store: CPStore, values: Iterable[Int]): CPVarInt = apply(store, values, "")

  @deprecated("Use apply(minValue: Int, maxValue: Int, name: String)(implicit store: CPStore) instead", "version alpha")
  def apply(store: CPStore, minValue: Int, maxValue: Int, name: String): CPVarInt = apply(minValue, maxValue, name)(store)

  @deprecated("Use apply(minValue: Int, maxValue: Int)(implicit store: CPStore) instead", "version alpha")
  def apply(store: CPStore, minValue: Int, maxValue: Int): CPVarInt = apply(minValue, maxValue, "")(store)

  @deprecated("Use apply(value: Int, name: String)(implicit store: CPStore) instead", "version alpha")
  def apply(store: CPStore, value: Int, name: String): CPVarInt = apply(value, name)(store)

  @deprecated("Use apply(value: Int)(implicit store: CPStore) instead", "version alpha")
  def apply(store: CPStore, value: Int): CPVarInt = apply(value, "")(store)

  /** Builds a CPVarInt from a range */
  private def rangeDomain(domain: Range, name: String, store: CPStore): CPVarInt = {
    if (domain.max - domain.min < domain.size - 1) iterableDomain(domain, name, store)
    else new CPVarIntImpl(store, domain.min, domain.max, name)
  }

  /** Builds a CPVarInt from an iterable */
  private def iterableDomain(domain: Iterable[Int], name: String, store: CPStore): CPVarInt = setDomain(domain.toSet, name, store)

  /** Builds a CPVarInt from an set */
  private def setDomain(domain: Set[Int], name: String, store: CPStore): CPVarInt = {
    val min = domain.min
    val max = domain.max
    val x = new CPVarIntImpl(store, min, max, name)
    if (max - min + 1 > domain.size) {
      for (v <- min to max if !domain.contains(v)) {
        x.removeValue(v)
      }
    }
    x
  }
}

  
