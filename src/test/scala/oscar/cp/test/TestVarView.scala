package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.modeling._

import org.scalacheck._

class TestVarView extends FunSuite with ShouldMatchers with CPModel {


  test("Table Var View 1") {
    val cp = CPSolver()
    var x = CPVarInt(cp, -2 to 4)
    
    class ViewCons(val X: CPVarInt) extends Constraint(X.getStore(), "TestView") {

    	var valRemove = true
      
    	override def setup(l: CPPropagStrength): CPOutcome = { 
    	    X.callValRemoveWhenValueIsRemoved(this)
    		X.callUpdateMinWhenMinChanges(this)
    		X.callUpdateMaxWhenMaxChanges(this)
    		X.callValBindWhenBind(this)
    		X.callUpdateBoundsWhenBoundsChange(this)
    		return CPOutcome.Suspend
    	}

    	override def valRemove(x: CPVarInt,v: Int): CPOutcome = {
          x should be(X)
          if (valRemove) {
           v should be(0)
           valRemove = false
          }  
          return CPOutcome.Suspend
        }
 
        override def valBind(x: CPVarInt): CPOutcome = {
          x should be(X)
          x.getValue() should be(-2)  
          return CPOutcome.Suspend
        }
 
        override def updateMin(x: CPVarInt, oldmin: Int): CPOutcome = {
          x should be(X)
          oldmin should be(-3)
          x.getMin() should be(-2)  
          return CPOutcome.Suspend
        }
        
        override def updateMax(x: CPVarInt, oldmax: Int): CPOutcome = {
          x should be(X)
          oldmax should be(3)
          x.getMax() should be(-2)
          return CPOutcome.Suspend
        }
        
        override def updateBounds(x: CPVarInt): CPOutcome = {
          x should be(X)
          return CPOutcome.Suspend
        }

    }
    
    val y = x+5+2-3-5 // y = x-1 so its domain should be -3..3
    cp.add(new ViewCons(y))
    cp.add(y != 0)
    cp.add(y >= -2)
    y.getMax() should be(3)
    y.getMin() should be(-2)
    cp.add(y <= -2) // now it's bind to -2
    y.getValue() should be(-2)
  }
  
  
  // same tests but with l1 indexes methods
  test("Table Var View 2") {
    val cp = CPSolver()
    var x = CPVarInt(cp, -2 to 4)
    
    class ViewCons(val X: CPVarInt) extends Constraint(X.getStore(), "TestView") {

    	var valRemove = true
      
    	override def setup(l: CPPropagStrength): CPOutcome = { 
    	    X.callValRemoveIdxWhenValueIsRemoved(this,-1)
    		X.callUpdateMinIdxWhenMinChanges(this,-1)
    		X.callUpdateMaxIdxWhenMaxChanges(this,-1)
    		X.callValBindIdxWhenBind(this,-1)
    		X.callUpdateBoundsIdxWhenBoundsChange(this,-1)
    		return CPOutcome.Suspend
    	}

    	override def valRemoveIdx(x: CPVarInt,idx: Int, v: Int): CPOutcome = {
          x should be(X)
          if (valRemove) {
           v should be(0)
           valRemove = false
          }  
          return CPOutcome.Suspend
        }
 
        override def valBindIdx(x: CPVarInt,idx: Int): CPOutcome = {
          x should be(X)
          x.getValue() should be(-2)  
          return CPOutcome.Suspend
        }
 
        override def updateMinIdx(x: CPVarInt,idx: Int, oldmin: Int): CPOutcome = {
          x should be(X)
          oldmin should be(-3)
          x.getMin() should be(-2)  
          return CPOutcome.Suspend
        }
        
        override def updateMaxIdx(x: CPVarInt,idx: Int,  oldmax: Int): CPOutcome = {
          x should be(X)
          oldmax should be(3)
          x.getMax() should be(-2)
          return CPOutcome.Suspend
        }
        
        override def updateBoundsIdx(x: CPVarInt,idx: Int): CPOutcome = {
          x should be(X)
          return CPOutcome.Suspend
        }

    }
    
    val y = x+5+2-3-5 // y = x-1 so its domain should be -3..3
    cp.add(new ViewCons(y))
    cp.add(y != 0)
    cp.add(y >= -2)
    y.getMax() should be(3)
    y.getMin() should be(-2)
    cp.add(y <= -2) // now it's bind to -2
    y.getValue() should be(-2)
  }  
  

}
