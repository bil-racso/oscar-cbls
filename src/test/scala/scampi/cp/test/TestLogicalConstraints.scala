package scampi.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scampi.cp.constraints._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.modeling._

import org.scalacheck._

class TestLogicalConstraints extends FunSuite with ShouldMatchers with CPModel {


  test("test 1") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarBool(cp))
    
    cp.post((x(0) && x(1)) || x(2))
    	
    cp.post(x(2) == 0)
    
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(0).getValue() should be(1)
    x(1).getValue() should be(1)

  }
  
  test("test 2") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarBool(cp))
    
    cp.post((x(0) && x(1)) || x(2))
    	
    cp.post(x(0) == 0)
    
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(2).getValue() should be(1)
    x(1).isBound() should be(false)

  }
  
  test("test 3") {
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarBool(cp))
    
    cp.post((x(0) && x(1)) && x(2))
  
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(0).getValue() should be(1)
    x(1).getValue() should be(1)
    x(2).getValue() should be(1)
    
  }
  

  

}