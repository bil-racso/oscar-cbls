package scampi.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scampi.cp.constraints._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.modeling._

import org.scalacheck._

class TestLogicalConstraints extends FunSuite with ShouldMatchers with CPModel {


  test("test logical 1") {
    
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarBool(cp))
    
    cp.post((x(0) && x(1)) || x(2))
    	
    cp.post(x(2) == 0)
    
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(0).getValue() should be(1)
    x(1).getValue() should be(1)

  }
  
  test("test logical 2") {
    
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarBool(cp))
    
    cp.post((x(0) && x(1)) || x(2))
    	
    cp.post(x(0) == 0)
    
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(2).getValue() should be(1)
    x(1).isBound() should be(false)

  }
  
  test("test logical 3") {
    
    val cp = CPSolver()
    var x = Array.fill(3)(CPVarBool(cp))
    
    cp.post((x(0) && x(1)) && x(2))
  
    cp.getStatus() should not be === (CPOutcome.Failure)
    x(0).getValue() should be(1)
    x(1).getValue() should be(1)
    x(2).getValue() should be(1)
    
  }
  
  test("test logical 4") {
    
    val cp = CPSolver()
    
    val A = CPVarBool(cp)
	val B = CPVarBool(cp)
	val C = CPVarBool(cp)
	val D = CPVarBool(cp)
	

	cp.add(((A ==> B) || C) && D)
	
	D.isTrue() should be(true)
    
	cp.add(A)
	cp.add(B == 0)
	
	C.isTrue() should be(true)
    
  }
  
  test("test logical 5") {
    
    val cp = CPSolver()
    
    val w = CPVarInt(cp,1 to 5)
    val x = CPVarInt(cp,1 to 5)
 
    val A = w <<= x

	cp.add(A)
	
	w.getMax() should be(4)
 
    cp.add(w <<= 2)
    w.getMax() should be(1)
    
  }  
  
  test("test logical 6") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,1 to 5)
    val z = CPVarInt(cp,1 to 5)
   
    cp.add(z >>= y)
    z.getMin() should be(2)
    
    cp.add((z >>= 3).constraintFalse()) // it means z <= 3
    z.getMax() should be(3)
    
  }   
  
  test("test logical 7") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,1 to 5)
    val z = CPVarInt(cp,3 to 5)
    val b = z === y
    
    cp.add(y <= 2)
    b.getValue() should be(0)
    
  } 
  
  test("test logical 8") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,Set(1,5,9))
    val z = CPVarInt(cp,Set(5,10,13))
    val b = z === y
    
    println(b)
    
    b.isBound() should be(false)
    
    cp.add(z != 5)
    
    b.getValue() should be(0)
    
  }
  
  test("test logical 9") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,Set(1,5,9))
    val z = CPVarInt(cp,Set(5,10,13))
    val b = z === y
    cp.add(b)
    
    y.getValue() should be(5)
    z.getValue() should be(5)
    
  }
  
  test("test logical 10") {
    
    val cp = CPSolver()
    
    val y = CPVarInt(cp,Set(1,5,9))
    val z = CPVarInt(cp,Set(5,10,13))
    val b = z === y
    cp.add(b.constraintFalse())
    cp.add(z == 5)
    y.hasValue(5) should be(false)
    
  }  
  

  

  

}