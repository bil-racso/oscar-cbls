package oscar.algo.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.algo.RangeMinQuery

class TestRangeMinQuery extends FunSuite with ShouldMatchers {

  test("test 1") {
    
    val values = Array(2,4,3,1,6,7,8,9,1,7)
    val rmq = new RangeMinQuery(values) 
    rmq(1,3) should be(3)
    rmq(2,5) should be(3)
    rmq(0,values.length-1) should (be(3) or be(8))

  }

 
}
