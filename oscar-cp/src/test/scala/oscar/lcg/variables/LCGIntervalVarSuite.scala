package oscar.lcg.variables

import oscar.lcg.testUtils._
import oscar.lcg.core.LCGSolver
import scala.util.Random

/** @author Renaud Hartert ren.hartert@gmail.com */
class LCGIntervalVarSuite extends TestSuite {

  // Returns true if the domain contains all the values between min and max
  private def containsAll(variable: LCGIntervalVar): Boolean = {
    val min = variable.min
    val max = variable.max
    (min to max).forall(variable.contains)
  }

  test("all values should be contained in the initial domain.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 10)(store)
    assert(variable.size == 6)
    variable shouldContain (5 to 10)
  }

  test("contains should return true if value is in the domain.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    variable shouldContain 5
    variable shouldContain 10
    variable shouldContain 15
  }

  test("contains should return false if value is not in the domain.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(!variable.contains(-1000))
    assert(!variable.contains(-10))
    assert(!variable.contains(4))
    assert(!variable.contains(16))
    assert(!variable.contains(20))
    assert(!variable.contains(1000))
  }

  test("setting a greaterEqual literal to True should adjust the minimum value and the size.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(variable.size == 11)
    val lit = variable.greaterEqual(10)
    assert(store.add(lit))
    assert(store.propagate())
    assert(variable.size == 6)
    assert(variable.min == 10)
  }

  test("setting a greaterEqual(newMin) literal to True should remove all values lesser than newMin.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(store.add(variable.greaterEqual(10)))
    assert(store.propagate())
    variable shouldContain (10 to 15)
    variable shouldNotContain (5 until 10)
  }

  test("setting a greaterEqual(newMin) literal to True with a lesser or equal than min should not impact the domain.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(store.add(variable.greaterEqual(4)))
    assert(store.propagate())
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
    assert(store.add(variable.greaterEqual(5)))
    assert(store.propagate())
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
  }

  test("setting a greaterEqual(max) literal to True should assign max.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(store.add(variable.greaterEqual(15)))
    assert(store.propagate())
    assert(variable.size == 1)
    assert(variable.isAssigned)
    assert(variable.contains(15))
    assert(variable.isAssignedTo(15))
    variable shouldNotContain (5 until 15)
  }

  test("setting a greaterEqual(value) literal to True with value greater than max should fail.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(!store.add(variable.greaterEqual(20)))
    //assert(!store.propagate())
  }

  test("setting a lowerEqual literal to True should adjust the maximum value and the size.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(variable.size == 11)
    assert(store.add(variable.lowerEqual(10)))
    assert(store.propagate())
    assert(variable.size == 6)
    assert(variable.max == 10)
  }

  test("setting a greaterEqual(newMax) literal to True should remove all values greater than newMax.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(store.add(variable.lowerEqual(10)))
    assert(store.propagate())
    assert(containsAll(variable))
  }

  test("setting a lowerEqual(newMax) literal to True with a greater or equal than max should not impact the domain.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(store.add(variable.lowerEqual(20)))
    assert(store.propagate())
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
    assert(store.add(variable.lowerEqual(15)))
    assert(store.propagate())
    assert(variable.size == 11)
    assert(variable.min == 5)
    assert(variable.max == 15)
  }

  test("setting a lowerEqual(min) literal to True should assign min.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(store.add(variable.lowerEqual(5)))
    assert(store.propagate())
    assert(variable.size == 1)
    assert(variable.isAssigned)
    assert(variable.contains(5))
    assert(variable.isAssignedTo(5))
    variable shouldNotContain (6 to 15)
  }

  test("setting a lowerEqual(value) literal to True with value lower than min should fail.") {
    val store = new LCGSolver()
    val variable = LCGIntervalVar(5, 15)(store)
    assert(!store.add(variable.lowerEqual(0)))
    //assert(store.propagate())
  }

  test("Bounds should be restored when a backtrack occurs") {
    val store = new LCGSolver()
    val context = store.cpStore
    val variable = LCGIntervalVar(5, 15)(store)
    context.pushState()
    assert(store.add(variable.lowerEqual(10)))
    assert(store.propagate())
    assert(variable.min == 5)
    assert(variable.max == 10)
    assert(variable.size == 6)
    assert(containsAll(variable))
    context.pushState()
    assert(store.add(variable.lowerEqual(9)))
    assert(store.add(variable.greaterEqual(6)))
    assert(store.propagate())
    assert(variable.min == 6)
    assert(variable.max == 9)
    assert(variable.size == 4)
    assert(containsAll(variable))
    context.pushState()
    context.pop()
    assert(variable.min == 6)
    assert(variable.max == 9)
    assert(variable.size == 4)
    assert(containsAll(variable))
    context.pop()
    assert(variable.min == 5)
    assert(variable.max == 10)
    assert(variable.size == 6)
    assert(containsAll(variable))
    context.pop()
    assert(variable.min == 5)
    assert(variable.max == 15)
    assert(variable.size == 11)
    assert(containsAll(variable))
  }
}