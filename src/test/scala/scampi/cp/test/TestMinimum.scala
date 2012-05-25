package scampi.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scampi.cp.constraints._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.modeling._

import org.scalacheck._

class TestMinimum extends FunSuite with ShouldMatchers with CPModel {


  test("Minimum1") {
    val cp = CPSolver()
    
    var x = Array.fill(3)(CPVarInt(cp, 1 to 3))
    
    var y = CPVarInt(cp, 0 to 6)
   
    cp.add(minimum(x) == y)
    
    y.getMin() should be(1)
    y.getMax() should be(3)
    
    x.foreach(w => cp.add(w >= 2))
    
    y.getMin() should be(2)


  }  
  
  test("Minimum2") {
    val cp = CPSolver()
    
    var x = Array.fill(3)(CPVarInt(cp, 1 to 3))
    
    var y = CPVarInt(cp, 0 to 6)
   
    cp.add(minimum(x) == y)
    
    cp.add(y >= 2)
    
    for (w <- x) {
      w.getMin() should be(2)
    }


  } 

}