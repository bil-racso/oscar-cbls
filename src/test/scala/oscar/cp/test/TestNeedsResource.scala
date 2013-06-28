package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.modeling.CPScheduler
import oscar.cp.scheduling.Activity
import oscar.cp.core.CPVarInt
import oscar.cp.scheduling.MaxResource

class TestNeedsResource extends FunSuite with ShouldMatchers {

/*
  // problematic test
  test("Test : TestNeedsResource1") {

    val instance = 
      Array(
    		(0,1,1),
    		(0,1,1),
    		(0,1,1),
    		(10,17,7),
    		(1,7,6),
    		(1,5,4),
    		(5,10,5),
    		(5,13,8),
    		(17,24,7),
    		(13,21,8),
    		(5,6,1),
    		(16,18,2),
    		(10,13,3),
    		(6,16,10),
    		(28,38,10),
    		(21,23,2),
    		(13,23,10),
    		(13,14,1),
    		(1,2,1),
    		(23,30,7),
    		(36,45,9),
    		(10,19,9),
    		(23,27,4),
    		(38,42,4),
    		(42,43,1),
    		(27,28,1),
    		(28,36,8),
    		(45,46,1),
    		(45,47,2),
    		(36,43,7)
    )
    
    val cp = CPScheduler(60)
    
    val activities = instance.map(i => new Activity(cp, CPVarInt(cp,i._1), CPVarInt(cp,i._3), CPVarInt(cp,i._2)))
    
    val resource = MaxResource(cp, 12)
    
    val makespan = cp.makespan

    var num_sol = 0
    
    cp.solve subjectTo {

    	activities(28) needs 0 ofResource resource
    	activities(29) needs 0 ofResource resource
    
    } exploration {
    	cp.binaryFirstFail(cp.activities)
    	num_sol += 1
    } run()
    cp.printStats()
    
    num_sol shouldBe 1

  }
  */
  
  
  test("Test : TestNeedsResource2") {

    val instance = 
      Array(
    		(0,1,1),
    		(0,1,1),
    		(0,1,1),
    		(10,17,7),
    		(1,7,6),
    		(1,5,4),
    		(5,10,5),
    		(5,13,8),
    		(17,24,7)
    )
    
    val cp = CPScheduler(60)
    
    val activities = instance.map(i => new Activity(cp, CPVarInt(cp,i._1), CPVarInt(cp,i._3), CPVarInt(cp,i._2)))
    
    val resource = MaxResource(cp, 12)
    
    val makespan = cp.makespan

    var num_sol = 0
    
    cp.solve subjectTo {
    
    } exploration {
    	cp.binaryFirstFail(cp.activities)
    	num_sol += 1
    } run()
    cp.printStats()
    
    num_sol shouldBe 1

  }  
}