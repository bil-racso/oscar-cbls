/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._


class TestVarView extends FunSuite with ShouldMatchers  {


  test("Table Var View 1") {
    val cp = CPSolver()
    var x = CPIntVar(-2 to 4)(cp)
    
    class ViewCons(val X: CPIntVar) extends Constraint(X.s, "TestView") {

    	var valRemove = true
      
    	override def setup(l: CPPropagStrength): CPOutcome = { 
    	    X.callValRemoveWhenValueIsRemoved(this)
    		X.callUpdateMinWhenMinChanges(this)
    		X.callUpdateMaxWhenMaxChanges(this)
    		X.callValBindWhenBind(this)
    		X.callUpdateBoundsWhenBoundsChange(this)
    		return CPOutcome.Suspend
    	}

    	override def valRemove(x: CPIntVar,v: Int): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          if (valRemove) {
           v should be(0)
           valRemove = false
          }  
          return CPOutcome.Suspend
        }
 
        override def valBind(x: CPIntVar): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          x.value should be(-2)  
          return CPOutcome.Suspend
        }
 
        override def updateMin(x: CPIntVar, oldmin: Int): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          oldmin should be(-3)
          x.min should be(-2)  
          return CPOutcome.Suspend
        }
        
        override def updateMax(x: CPIntVar, oldmax: Int): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          oldmax should be(3)
          x.max should be(-2)
          return CPOutcome.Suspend
        }
        
        override def updateBounds(x: CPIntVar): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          return CPOutcome.Suspend
        }

    }
    
    val y = x+5+2-3-5 // y = x-1 so its domain should be -3..3
    cp.add(new ViewCons(y))
    cp.add(y != 0)
    cp.add(y >= -2)
    y.max should be(3)
    y.min should be(-2)
    cp.add(y <= -2) // now it's bind to -2
    y.value should be(-2)
  }
  
  
  // same tests but with l1 indexes methods
  test("Table Var View 2") {
    val cp = CPSolver()
    var x = CPIntVar(-2 to 4)(cp)
    
    class ViewCons(val X: CPIntVar) extends Constraint(X.s, "TestView") {

    	var valRemove = true
      
    	override def setup(l: CPPropagStrength): CPOutcome = { 
    	    X.callValRemoveIdxWhenValueIsRemoved(this,-1)
    		X.callUpdateMinIdxWhenMinChanges(this,-1)
    		X.callUpdateMaxIdxWhenMaxChanges(this,-1)
    		X.callValBindIdxWhenBind(this,-1)
    		X.callUpdateBoundsIdxWhenBoundsChange(this,-1)
    		return CPOutcome.Suspend
    	}

    	override def valRemoveIdx(x: CPIntVar,idx: Int, v: Int): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          if (valRemove) {
           v should be(0)
           valRemove = false
          }  
          return CPOutcome.Suspend
        }
 
        override def valBindIdx(x: CPIntVar,idx: Int): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          x.value should be(-2)  
          return CPOutcome.Suspend
        }
 
        override def updateMinIdx(x: CPIntVar,idx: Int, oldmin: Int): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          oldmin should be(-3)
          x.min should be(-2)  
          return CPOutcome.Suspend
        }
        
        override def updateMaxIdx(x: CPIntVar,idx: Int,  oldmax: Int): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          oldmax should be(3)
          x.max should be(-2)
          return CPOutcome.Suspend
        }
        
        override def updateBoundsIdx(x: CPIntVar,idx: Int): CPOutcome = {
    	  val eq: Boolean = x==X
    	  eq should  equal(true)
          return CPOutcome.Suspend
        }

    }
    
    val y = x+5+2-3-5 // y = x-1 so its domain should be -3..3
    cp.add(new ViewCons(y))
    cp.add(y != 0)
    cp.add(y >= -2)
    y.max should be(3)
    y.min should be(-2)
    cp.add(y <= -2) // now it's bind to -2
    y.value should be(-2)
  }
  
   
  

}
