package oscar.lcg.core

import oscar.lcg.testUtils._

class LiftedBooleanSuite extends TestSuite {

  test("opposite of True should be False.") {
    assert(True.opposite == False)
  }
  
  test("opposite of False should be True.") {
    assert(False.opposite == True)
  }
    
  test("opposite of Unassigned should be Unassigned.") {
    assert(Unassigned.opposite == Unassigned)
  }
}