package oscar.cp.constraints

import oscar.cp.TestSuite
import oscar.cp.core.CPIntVar
import oscar.cp.modeling.CPSolver

class SubCircuitSuite extends TestSuite {
  
  private def testData(nSuccs: Int): (CPSolver, Array[CPIntVar]) = {
    val cp = CPSolver()
    val succs = Array.fill(nSuccs)(CPIntVar(0 until nSuccs)(cp))
    cp.post(SubCircuit(succs))
    (cp, succs)
  }
  
  test("one") {
    val (cp, succs) = testData(1)
    assert(succs.head.value == 0)
    assert(!cp.isFailed)
  }
  
  test("no circuit") {
    val (cp, succs) = testData(10)
    for (i <- 0 until 10) {
      cp.add(succs(i) == i)
    }
    assert(!cp.isFailed)
  }
  
  test("circuit") {
    val (cp, succs) = testData(5)
    cp.add(succs(0) == 1)
    cp.add(succs(1) == 2)
    cp.add(succs(2) == 3)
    cp.add(succs(3) == 4)
    cp.add(succs(4) == 0)
    assert(!cp.isFailed)
  } 
  
  test("subcircuit 1") {
    val (cp, succs) = testData(5)
    cp.add(succs(0) == 1)
    cp.add(succs(1) == 2)
    cp.add(succs(2) == 0)
    assert(!cp.isFailed)
    assert(succs(3).value == 3)
    assert(succs(4).value == 4)
  }
  
  test("subcircuit 2") {
    val (cp, succs) = testData(5)
    cp.add(succs(3) == 3)
    cp.add(succs(0) == 1)
    cp.add(succs(1) == 2)
    cp.add(succs(2) == 0)
    assert(!cp.isFailed)
    assert(succs(4).value == 4)
  }
  
  test("only one subtour") {
    val (cp, succs) = testData(5)
    cp.add(succs(0) == 1)
    cp.add(succs(1) == 2)
    cp.add(succs(3) == 4)
    assert(!cp.isFailed)
    assert(succs(4).value == 0)
    assert(succs(2).value == 3)
  }
}