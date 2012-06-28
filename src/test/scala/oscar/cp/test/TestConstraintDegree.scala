package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._

import org.scalacheck._

class TestConstraintDegree extends FunSuite with ShouldMatchers with CPModel {


  test("ConstraintDegree1") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarInt(cp, 1 to 3))
    
    cp.post(x(0) != x(1))
    cp.post(x(0) != x(2))
    x(0).getConstraintDegree() should be(2)
    x(1).getConstraintDegree() should be(1)
    	
  }
  


}
