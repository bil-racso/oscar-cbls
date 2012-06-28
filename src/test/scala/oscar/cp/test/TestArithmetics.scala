package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._

import org.scalacheck._

class TestArithmetics extends FunSuite with ShouldMatchers with CPModel {


  test("Arithmetics") {
    val cp = CPSolver()
    val i = CPVarInt(cp,1)
    val j = CPVarInt(cp,0)
    val a = CPVarInt(cp,-1 to 1)
    val b = CPVarInt(cp,0 to 1)
    val n = 8
    
    val ia = i+a
    val jb = j+b
    
        
    cp.add(ia >= 0)
    cp.add(ia <  n)
    cp.add(jb >= 0)
    cp.add(jb <  n)

    println(ia)
    println(jb)
    val ix2 = (ia)*n + (jb) // what is the index of k+1
    
    println(ix2)
    ix2.getSize() should be(6) // should contain: 0,1,8,16,17

  }  
  
  

}
