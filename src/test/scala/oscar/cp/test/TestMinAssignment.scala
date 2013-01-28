package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.core._
import oscar.cp.modeling._
import oscar.cp.constraints.MinAssignment2

class TestMinAssignment extends FunSuite with ShouldMatchers {

  test("Test Assignment 1") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPVarInt(cp, 0 to 100)
    val x = Array.fill(3)(CPVarInt(cp, 0 to 2))
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(7)
    cp.post(x(0) != 0)
    cost.min should be(8)
    cp.post(x(2) != 2)
    cost.min should be(10)
  }
  

  test("Test Assignment 2") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPVarInt(cp, 0 to 100)
    val x = Array(CPVarInt(cp, Set(1,2)),CPVarInt(cp, Set(0,1,2)),CPVarInt(cp, Set(0,1)))
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(10)
  }
  
  
  test("Test Assignment 3") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPVarInt(cp, 0 to 100)
    val x = Array(CPVarInt(cp, Set(0,1,2)),CPVarInt(cp, Set(0,1,2)),CPVarInt(cp, Set(0,1)))
    cp.post(new MinAssignment(x, w, cost))
    println(cost)
    cost.min should be(10)
  }  
  
  test("Test Assignment 4") {
    val cp = CPSolver()
    val w = Array(Array(2, 3, 4), 
                  Array(3, 3, 3), 
                  Array(4, 5, 2))
    val cost = CPVarInt(cp, 0 to 100)
    val x = Array.fill(3)(CPVarInt(cp, 0 to 2))
    cp.post(new MinAssignment(x, w, cost))
    cost.min should be(7)
    cp.pushState()
    cp.post(x(0) != 0)
    cost.min should be(8)
    cp.pop()
    cp.post(x(2) != 2)
    cost.min should be(10)
  }
  
   

}
