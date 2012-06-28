package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._

import org.scalacheck._

class TestTableSTR2 extends FunSuite with ShouldMatchers with CPModel {


  test("Table Test 1") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarInt(cp, 1 to 3))
    
    val tuples = Array(Array(1,1,1),Array(1,2,3))
    

    cp.post(new TableSTR2(x,tuples))
    	
    x(0).isBound() should be(true)
    x(0).getValue() should be(1)
    x(2).hasValue(2) should be (false)
    
    cp.post(x(2) != 3)
    
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(1).getValue() should be(1)
    x(2).getValue() should be(1)

  }
  
  test("Table Test 2") {
    val cp = CPSolver()
    
    var x = CPVarInt(cp, 0 to 4)
    var y = CPVarInt(cp, 0 to 4)
    var z = CPVarInt(cp, 0 to 24)
    
    
    val tuples = (for (i <- 0 until 5; j <- i+1 until 5) yield Array(i,j,i*4+j-1)).toArray
    cp.post(new TableSTR2(Array(x,y,z),tuples))
    cp.post(z == 0)
    x.getValue() should be(0)
    y.getValue() should be(1)
    z.getValue() should be(0)

  }
  
  test("Table Test 3") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarInt(cp, 1 to 7))
    
    val tuples = Array(Array(1,1,1),Array(1,2,3),Array(1,2,7),Array(2,1,4))
    

    
    
    var nbSol = 0
    	
    cp.solveAll subjectTo {
      cp.add(new TableSTR2(x,tuples))
    } exploration {
      cp.binary(x)
      nbSol += 1
    }
    nbSol should be(4)

  }
  
    test("Table Test 4") {
    val cp = CPSolver()
    var x = Array.fill(2)(CPVarInt(cp, 1 to 1))
    
    val tuples = Array(Array(1,2),Array(2,1))
    

    cp.post(new TableSTR2(x,tuples))
    cp.isFailed should be(true)
    

  }
  

}
