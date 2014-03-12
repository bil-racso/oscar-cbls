package oscar.cp.core.domains

import oscar.cp.TestSuite
import oscar.cp.constraints._
import oscar.cp.modeling._
import collection.immutable.SortedSet

class IntDomainSuite extends TestSuite {

  test("Test Dom1") {

    val cp = CPSolver()
    val domain = AdaptableIntDomain(cp, -5, 5)
    def dom = domain.value
    
    val oldSize = dom.size
    val oldMin = dom.min
    val oldMax = dom.max

    dom.updateMax(4)
    dom.updateMin(0)
    dom.delta(oldMin, oldMax, oldSize).toSet should be(Set(-5, -4, -3, -2, -1, 5))

    dom.removeValue(2)
    dom.removeValue(3)
    dom.delta(oldMin, oldMax, oldSize).toSet should be(Set(-5, -4, -3, -2, -1, 2, 3, 5))
  }

  test("Test Dom2") {

    val cp = CPSolver()
    val domain = AdaptableIntDomain(cp, -5, 5)
    def dom = domain.value
    
    val oldSize = dom.size
    val oldMin = dom.min
    val oldMax = dom.max

    dom.updateMax(10)
    dom.updateMin(-10)

    dom.updateMax(2)
    dom.updateMin(-2)
    dom.delta(oldMin, oldMax, oldSize).toSet should be(Set(-5, -4, -3, 3, 4, 5))

    dom.removeValue(2)
    dom.removeValue(3)
    dom.delta(oldMin, oldMax, oldSize).toSet should be(Set(-5, -4, -3, 2, 3, 4, 5))

    dom.size should be(4)
    dom.min should be(-2)
    dom.max should be(1)

    dom.removeValue(0)
    dom.size should be(3)
    dom.min should be(-2)
    dom.max should be(1)

    dom.delta(oldMin, oldMax, oldSize).toSet should be(Set(-5, -4, -3, 0, 2, 3, 4, 5))
  }

  test("Test Dom3") {

    val cp = CPSolver()
    val domain = AdaptableIntDomain(cp, -5, 5)
    def dom = domain.value
    
    val oldSize = dom.size
    val oldMin = dom.min
    val oldMax = dom.max

    dom.assign(-2)
    dom.delta(oldMin, oldMax, oldSize).toSet should be(Set(-5, -4, -3, -1, 0, 1, 2, 3, 4, 5))
    dom.size should be(1)
    dom.min should be(-2)
    dom.max should be(-2)
  }

  test("Test Dom4") {

    val cp = CPSolver()
    val domain = AdaptableIntDomain(cp, -5, 5)
    def dom = domain.value

    dom.removeValue(-1)
    dom.removeValue(0)
    dom.removeValue(1)
    dom.nextValue(-2) should be(-2)
    dom.nextValue(-1) should be(2)
    dom.prevValue(2) should be(2)
    dom.prevValue(1) should be(-2)
  }
}