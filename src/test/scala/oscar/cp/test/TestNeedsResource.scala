package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.modeling.CPScheduler
import oscar.cp.scheduling.Activity
import oscar.cp.core.CPVarInt
import oscar.cp.scheduling.MaxResource

class TestNeedsResource extends FunSuite with ShouldMatchers {


  /*Test that checks big ints are used properly in QuadraticCumulativeEdgeFinding.scala 
   *(in this case particular case : not using a BigInt implies getting the following wrong computation : 2147483603 * 12 = -540)
   */
	test("Test : TestNeedsResourceBigInt")
	{

    val instance = 
      Array(
    		(45,47,2),
    		(36,43,7)
    )
    
    val cp = CPScheduler(60)
    
    val activities = instance.map(i => new Activity(cp, CPVarInt(cp,i._1), CPVarInt(cp,i._3), CPVarInt(cp,i._2)))
    
    val resource = MaxResource(cp, 12)
    
    val makespan = cp.makespan

    var num_sol = 0
    
    cp.solve subjectTo {
    
      activities(0) needs 0 ofResource resource
      activities(1) needs 0 ofResource resource
    
    } exploration {
    	cp.binaryFirstFail(cp.activities)
    	num_sol += 1
    } run()
    cp.printStats()
    
    num_sol shouldBe 1

  }
  
  
  
  test("Test : TestNeedsResourceNoConstraint") {

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