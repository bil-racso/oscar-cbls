package oscar.cp.constraints

import oscar.cp.testUtils._
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPIntVar


class IntDivisionACSuite extends TestSuite {
  
  test("an IllegalArgumentException should be thrown if c <= 0") {
    implicit val solver = CPSolver()
    val a = CPIntVar(1 to 10)
    val b = CPIntVar(1 to 10)
    intercept[IllegalArgumentException] {
      new IntDivisionAC(a, b, 0)
    }
    intercept[IllegalArgumentException] {
      new IntDivisionAC(a, b, -10)
    }
  }
  
  test("values of a that have initially no support in b should be removed") {
    implicit val solver = CPSolver()
    val values = (1 to 10)
    val a = CPIntVar(values)
    val b = CPIntVar(Set(2, 10, 11, 13, 15, 17, 34))
    solver.post(new IntDivisionAC(a, b, 3))
    assert(!solver.isFailed)
    val contained = Array(3, 4, 5)
    val removed = values.filter(!contained.contains(_))
    for (v <- contained) {
      assert(a.hasValue(v), v + " should be in " + a)
    }
    for (v <- removed) {
      assert(!a.hasValue(v), v + " should not be in " + a)
    }
  }
  
  test("values of b that have initially no support in a should be removed") {
    implicit val solver = CPSolver()
    val values = (0 to 20)
    val a = CPIntVar(Set(1, 4, 5, 9))
    val b = CPIntVar(values)
    solver.post(new IntDivisionAC(a, b, 3))
    assert(!solver.isFailed)
    val contained = Set(3, 4, 5, 12, 13, 14, 15, 16, 17)
    val removed = values.filter(!contained.contains(_))
    for (v <- contained) {
      assert(b.hasValue(v), v + " should be in " + b)
    }
    for (v <- removed) {
      assert(!b.hasValue(v), v + " should not be in " + b)
    }
  }
  
  test("a should be equal to b if c equals 1") {
    implicit val solver = CPSolver()
    val a = CPIntVar(-10 to 10)
    val b = CPIntVar(-5 to 15)
    solver.post(new IntDivisionAC(a, b, 1)) 
    assert(!solver.isFailed)
    for (v <- a) {
      assert(b.hasValue(v), v + " should be in " + b)
    }
    for (v <- b) {     
      assert(a.hasValue(v), v + " should be in " + a)
    }
  }
  
  test("b should be reduced to [-c+1; c-1] if a is initially equal to 0") {
    implicit val solver = CPSolver()
    val values = -10 to 10
    val a = CPIntVar(0)
    val b = CPIntVar(values)
    val c = 3
    solver.post(new IntDivisionAC(a, b, c)) 
    assert(!solver.isFailed)
    val contained = ((-c+1) until c).toSet
    val removed = values.filter(!contained.contains(_))
    for (v <- contained) {
      assert(b.hasValue(v), v + " should be in " + b)
    }
    for (v <- removed) {
      assert(!b.hasValue(v), v + " should not be in " + b)
    }
  }
  
  test("the constraint should fail if a has no support in b") {
    implicit val solver = CPSolver()
    val a = CPIntVar(Set(1, 4, 5, 9))
    val b = CPIntVar(Set(2, 6, 7, 11, 18))
    solver.post(new IntDivisionAC(a, b, 3)) 
    assert(solver.isFailed)
  }
  
  test("values in [v*c; v*c+c[ should be removed from b when v is removed from a") {
    implicit val solver = CPSolver()
    val values = -15 to 15
    val a = CPIntVar(-5 to 5)
    val b = CPIntVar(values)
    val c = 3
    solver.post(new IntDivisionAC(a, b, c)) 
    assert(!solver.isFailed)
    def removeAndCheck(v: Int): Unit = {     
      solver.post(a != v)
      for (i <- (v*c) until (v*c+c)) {
        assert(!b.hasValue(i), i + " should not be in " + b)
      }
    }
    removeAndCheck(-4)
    removeAndCheck(0)
    removeAndCheck(3)
  } 
  
  test("v/c should be removed from a when v is removed from b") {
    implicit val solver = CPSolver()
    val values = -15 to 15
    val a = CPIntVar(-5 to 5)
    val b = CPIntVar(values)
    val c = 3
    solver.post(new IntDivisionAC(a, b, c)) 
    assert(!solver.isFailed)
    def removeAndCheck(v: Int): Unit = {     
      solver.post(b != v)
      assert(!a.hasValue(v/4), (v/4) + " should not be in " + a)
    }
    removeAndCheck(-14)
    removeAndCheck(-13)
    removeAndCheck(-6)
    removeAndCheck(0)
    removeAndCheck(3)
    removeAndCheck(7)
  } 
}