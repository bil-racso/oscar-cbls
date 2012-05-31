package scampi.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scampi.cp.constraints._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.modeling._

import org.scalacheck._

class TestStretchWrapper extends FunSuite with ShouldMatchers with CPModel {


  test("test stretch 1") {
    val cp = CPSolver()
    var x = Array.fill(6)(CPVarInt(cp,0 to 2))
    val automaton = stretchAutomaton(x, 2, 2)
    cp.add(regular(x,automaton))
    cp.add(x(5) == 0)
    cp.getStatus() should not be === (CPOutcome.Failure)
    
    //  0-2  0-2  1-2  1-2  0   0
    println(x.mkString(","))
    x(4).getValue() should be(0)
    x(3).hasValue(0) should be(false)
    x(2).hasValue(0) should be(false)
      
  }
  
 
  

  

}