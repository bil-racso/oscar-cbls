package oscar.cp.core.domains

import oscar.cp.TestSuite
import oscar.algo.reversible.ReversibleContext
import oscar.cp.core.CPOutcome._

/**
 *  Tests the sparse set implementation of a sparse domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
// Test the sparse set implementation of a sparse domain
class SparseSetDomainSuite extends SparseIntDomainSuite {
  override def sparseDomain(context: ReversibleContext, minValue: Int, maxValue: Int): SparseIntDomain = {
    new SparseSetDomain(context, minValue, maxValue)
  }
}

/**
 *  Test the bit vector implementation of a sparse domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class SingleBitVectorDomainSuite extends SparseIntDomainSuite {
  override def sparseDomain(context: ReversibleContext, minValue: Int, maxValue: Int): SparseIntDomain = {
    new SingleBitVectorDomain(context, minValue, maxValue)
  }
}

/**
 *  Generic class to test implementations of a sparse domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class SparseIntDomainSuite extends IntervalIntDomainSuite {

  // Implement this method to test your implementation of sparse domain
  def sparseDomain(context: ReversibleContext, minValue: Int, maxValue: Int): SparseIntDomain

  // Used to test interval functions
  override def intervalDomain(context: ReversibleContext, minValue: Int, maxValue: Int): IntervalIntDomain = {
    sparseDomain(context, minValue, maxValue)
  }

  test("Removed values are not contained in the domain anymore") {
    val context = new ReversibleContext()
    val domain = sparseDomain(context, 5, 10)
    assert(domain.removeValue(5) == Suspend)
    assert(!domain.hasValue(5))
    assert(domain.removeValue(7) == Suspend)
    assert(!domain.hasValue(7))
    assert(domain.removeValue(8) == Suspend)
    assert(!domain.hasValue(8))
  }

  test("Remove a value should reduce the size") {
    val context = new ReversibleContext()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    assert(domain.removeValue(5) == Suspend)
    assert(domain.size == size - 1)
    assert(domain.removeValue(5) == Suspend)
    assert(domain.size == size - 1)
    assert(domain.removeValue(6) == Suspend)
    assert(domain.size == size - 2)
  }

  test("Remove a removed value should not impact the domain") {
    val context = new ReversibleContext()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    assert(domain.removeValue(4) == Suspend)
    assert(domain.size == size)
    assert(domain.removeValue(11) == Suspend)
    assert(domain.size == size)

  }

  test("Remove the minimal value change the minimal value") {
    val context = new ReversibleContext()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    assert(domain.removeValue(5) == Suspend)
    assert(domain.min == 6)
    assert(domain.removeValue(6) == Suspend)
    assert(domain.removeValue(7) == Suspend)
    assert(domain.min == 8)
    assert(domain.removeValue(10) == Suspend)
    assert(domain.min == 8)
  }

  test("Remove all but one value assigns this value") {
    val context = new ReversibleContext()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    assert(domain.removeValue(5) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.removeValue(6) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.removeValue(9) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.removeValue(10) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.removeValue(8) == Suspend)
    assert(domain.hasValue(7))
    assert(domain.isBound)
  }

  test("Removed values are restored when a backtrack occurs") {
    val context = new ReversibleContext()
    val domain = sparseDomain(context, 5, 10)
    val size = domain.size
    context.pushState()
    assert(domain.removeValue(5) == Suspend)
    assert(domain.removeValue(6) == Suspend)
    context.pushState()
    assert(domain.removeValue(9) == Suspend)
    context.pushState()
    assert(domain.removeValue(8) == Suspend)
    assert(!domain.hasValue(5))
    assert(!domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(!domain.hasValue(8))
    assert(!domain.hasValue(9))
    assert(domain.hasValue(10))
    context.pop()
    assert(!domain.hasValue(5))
    assert(!domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(domain.hasValue(8))
    assert(!domain.hasValue(9))
    assert(domain.hasValue(10))
    context.pop()
    assert(!domain.hasValue(5))
    assert(!domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(domain.hasValue(8))
    assert(domain.hasValue(9))
    assert(domain.hasValue(10))
    context.pop()
    assert(domain.hasValue(5))
    assert(domain.hasValue(6))
    assert(domain.hasValue(7))
    assert(domain.hasValue(8))
    assert(domain.hasValue(9))
    assert(domain.hasValue(10))
  }
}

object TestApp extends App {
  println("test")
  val context = new ReversibleContext()
  val domain = new SingleBitVectorDomain(context, 5, 10)
  val size = domain.size
  assert(domain.removeValue(5) == Suspend)
  assert(domain.hasValue(7))
  assert(domain.removeValue(6) == Suspend)
  assert(domain.hasValue(7))
  assert(domain.removeValue(9) == Suspend)
  assert(domain.hasValue(7))
  assert(domain.removeValue(10) == Suspend)
  assert(domain.hasValue(7))
  assert(domain.removeValue(8) == Suspend)
  assert(domain.hasValue(7))
  assert(domain.isBound)
}