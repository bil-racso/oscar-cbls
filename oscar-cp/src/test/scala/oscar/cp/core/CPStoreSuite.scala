package oscar.cp.core

import oscar.cp.testUtils.TestSuite
import oscar.cp.core.variables.CPIntVar

class CPStoreSuite extends TestSuite {

  test("inconsistent assign should make the store inconsistent") {
    val store = new CPStore()
    val variable = CPIntVar(1 to 10)(store)
    store.assign(variable, 11)
    assert(store.isFailed)
  }
  
  test("inconsistent remove should make the store inconsistent") {
    val store = new CPStore()
    val variable = CPIntVar(1 to 1)(store)
    store.remove(variable, 1)
    assert(store.isFailed)
  }
}