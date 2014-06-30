package oscar.cp.core.domains

import oscar.cp.TestSuite
import oscar.algo.reversible.ReversibleContext

/**
 *  Test the bound implementation of an interval domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
class IntervalDomainSuite extends IntervalIntDomainSuite {
  override def intervalDomain(context: ReversibleContext, minValue: Int, maxValue: Int): IntervalIntDomain = {
    new BoundDomain(context, minValue, maxValue)
  }
}

/**
 *  Generic class to test implementations of an interval domain
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class IntervalIntDomainSuite extends TestSuite {
  
  // Implement this method to test your implementation of interval domain
  def intervalDomain(context: ReversibleContext, minValue: Int, maxValue: Int): IntervalIntDomain
  
  // TODO: add more tests
  
  test("All values should be contained in the initial domain") {
    val context = new ReversibleContext()
    val domain = intervalDomain(context, 5, 10)
    assert(domain.size == 6)
    assert((5 to 10).forall(domain.hasValue(_)))
  }
}