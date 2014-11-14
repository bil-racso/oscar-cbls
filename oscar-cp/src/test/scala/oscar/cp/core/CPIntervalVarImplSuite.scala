package oscar.cp.core

import oscar.cp.minizinc.test
import oscar.algo.reversible.ReversibleContext
import oscar.cp.core.domains.IntervalDomain
import scala.util.Random
import oscar.cp.testUtils.TestSuite
import oscar.cp.core.CPOutcome._

class CPIntervalVarImplSuite extends TestSuite {
  
  // Returns true if the domain contains all the values between min and max
  private def containsAll(variable: CPIntervalVar): Boolean = {
    val min = variable.min
    val max = variable.max
    (min to max).forall(variable.hasValue)
  }
  
  test("All values should be contained in the initial domain") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 10)
    assert(variable.size == 6)
    assert((5 to 10).forall(variable.hasValue))
  }
  
  test("HasValue should return true if value is in the domain") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.hasValue(5))
    assert(variable.hasValue(10))
    assert(variable.hasValue(15))
  }
  
  test("HasValue should return false if value is not in the domain") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(!variable.hasValue(-1000))
    assert(!variable.hasValue(-10))
    assert(!variable.hasValue(4))
    assert(!variable.hasValue(16))
    assert(!variable.hasValue(20))
    assert(!variable.hasValue(1000))
  }
  
  test("UpdateMin should adjust the minimum value and the size") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.size == 11)
    assert(variable.updateMin(10) == Suspend)
    assert(variable.size == 6)
    assert(variable.min == 10)
  }
  
  test("UpdateMin should remove all values lesser than min") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.updateMin(10) == Suspend)
    assert(containsAll(variable))
  }
  
  test("UpdateMin with a lesser or equal value than min should not impact the domain") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.updateMin(4) == Suspend)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
    assert(variable.updateMin(5) == Suspend)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
  }
  
  test("UpdateMin to max should assign max") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.updateMin(15) == Suspend)
    assert(variable.size == 1)
    assert(variable.isBound)
    assert(variable.hasValue(15))
  }
  
  test("UpdateMin greater than max should fail") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    intercept[Inconsistency](variable.updateMin(20))
  }
  
  test("UpdateMax should adjust the maximum value and the size") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.size == 11)
    assert(variable.updateMax(10) == Suspend)
    assert(variable.size == 6)
    assert(variable.max == 10)
  }
  
  test("UpdateMax should remove all values greater than max") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.updateMax(10) == Suspend)
    assert(containsAll(variable))
  }
  
  test("UpdateMax with a greater or equal value than max should not impact the domain") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.updateMax(20) == Suspend)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
    assert(variable.updateMax(15) == Suspend)
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
  }
  
  test("UpdateMax to min should assign min") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.updateMax(5) == Suspend)
    assert(variable.size == 1)
    assert(variable.isBound)
    assert(variable.hasValue(5))
  }
  
  test("UpdateMax lesser than min should fail") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    intercept[Inconsistency](variable.updateMax(0))
  }
  
  test("Bounds should be restored when a backtrack occurs") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    store.pushState()
    assert(variable.updateMax(10) == Suspend)
    assert(variable.min == 5)
    assert(variable.max == 10)
    assert(variable.size == 6)
    assert(containsAll(variable))
    store.pushState()
    assert(variable.updateMax(9) == Suspend)
    assert(variable.updateMin(6) == Suspend)
    assert(variable.min == 6)
    assert(variable.max == 9)
    assert(variable.size == 4)
    assert(containsAll(variable))
    store.pushState()
    store.pop()
    assert(variable.min == 6)
    assert(variable.max == 9)
    assert(variable.size == 4)
    assert(containsAll(variable))
    store.pop()
    assert(variable.min == 5)
    assert(variable.max == 10)
    assert(variable.size == 6)
    assert(containsAll(variable))
    store.pop()
    assert(variable.min == 5)
    assert(variable.max == 15)
    assert(variable.size == 11)
    assert(containsAll(variable))
  }
  
  test("Assign should make min equal to max") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.assign(10) == Suspend)
    assert(variable.hasValue(10))
    assert(variable.min == 10)
    assert(variable.max == 10)
  }
  
  test("Assign should reduce the size to 1") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    assert(variable.assign(10) == Suspend)
    assert(variable.size == 1)
  }
  
  test("Assign an out of bounds value should fail") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    intercept[Inconsistency](variable.assign(20))
  }
  
  test("Random values should be contained in the domain") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 10, 30)
    for (seed <- 1 to 10) {
      val rand = new Random(seed)
      for (i <- 1 to 20) {
        val value = variable.randomValue(rand)
        assert(variable.hasValue(value), s"$value is not in the domain")
      }
    }
  }
  
  test("Random values should always be the assigned value when the size is 1") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 10, 10)
    for (seed <- 1 to 10) {
      val rand = new Random(seed)
      for (i <- 1 to 20) {
        val value = variable.randomValue(rand)
        assert(value == 10, s"$value is not the assigned value")
      }
    }
  }

  test("Iterator should iterate on all the values") {
    val store = new CPStore()
    val variable = CPIntervalVarImpl(store, 5, 15)
    val values1 = (5 to 15).toSet
    assert(variable.iterator.forall(variable.hasValue(_)))
    assert(variable.iterator.forall(values1.contains(_)))
    assert(variable.iterator.size == 11)
    val values2 = (7 to 10).toSet
    assert(variable.updateMin(7) == Suspend)
    assert(variable.updateMax(10) == Suspend)
    assert(variable.size == 4)    
    assert(variable.iterator.forall(variable.hasValue(_)))
    assert(variable.iterator.forall(values2.contains(_)))
    assert(variable.iterator.size == 4)
  }
}