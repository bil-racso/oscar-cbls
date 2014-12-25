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
import oscar.cp._
import collection.immutable.SortedSet
import java.util.LinkedList
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestDeltaPropagate extends FunSuite with ShouldMatchers {
  
  test("test delta 1") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {

      override def setup(l: CPPropagStrength): CPOutcome = {
        X.callPropagateWhenDomainChanges(this,true)
        CPOutcome.Suspend
      }
      
      override def propagate(): CPOutcome = {
        //println(X.delta(this).toSet)
        
        X.changed(this) should be(true)
        X.deltaSize(this) should be(2)
        X.delta(this).toSet should be(Set(5,7))
        X.maxChanged(this) should be(true)
        X.minChanged(this) should be(false)
        X.oldMin(this) should be(1)
        X.oldMax(this) should be(7)
        
        
        propag = true
        CPOutcome.Suspend
      }
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(1, 3, 5, 7))(cp)
    cp.add(new MyCons(x))
    cp.add(x < 5)
    propag should be(true)
  }
  


  test("test delta 2") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {

      override def setup(l: CPPropagStrength): CPOutcome = {
        X.callPropagateWhenDomainChanges(this,trackDelta = true)
        CPOutcome.Suspend
      }
      
      override def propagate(): CPOutcome = {
        //println(X.delta(this).toSet)
        
        X.changed(this) should be(true)
        X.deltaSize(this) should be(2)
        X.delta(this).toSet should be(Set(2,4))
        X.maxChanged(this) should be(true)
        X.minChanged(this) should be(false)
        X.oldMin(this) should be(-2)
        X.oldMax(this) should be(4)
        
        
        propag = true
        CPOutcome.Suspend
      }
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2 // -2,0,2,4
    cp.add(new MyCons(x))
    cp.add(x < 2)
    propag should be(true)
  }
  
  test("test delta 3") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MAXPRIORL2-5 
      override def setup(l: CPPropagStrength): CPOutcome = {
        X.callPropagateWhenDomainChanges(this,true)
        CPOutcome.Suspend
      }
      
      override def propagate(): CPOutcome = {
        //println(X.delta(this).toSet)
        
        X.changed(this) should be(true)
        X.deltaSize(this) should be(2)
        X.delta(this).toSet should be(Set(2,4))
        X.maxChanged(this) should be(true)
        X.minChanged(this) should be(false)
        X.oldMin(this) should be(-2)
        X.oldMax(this) should be(4)
        
        
        propag = true
        CPOutcome.Suspend
      }
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2 // -2,0,2,4
    cp.add(new MyCons(x))
    val cons = new LinkedList[Constraint]()
    cons.add(x < 4)
    cons.add(x < 2)
    cp.add(cons)
    //println("x dom:"+x.toSet)
    propag should be(true)
  }    
  



  test("test delta 4") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MAXPRIORL2-5 
      override def setup(l: CPPropagStrength): CPOutcome = {
        X.filterWhenDomainChanges { delta =>
          propag = true
          delta.changed() should be(true)
          delta.size() should be(2)
          delta.values().toSet should be(Set(2,4))
          delta.maxChanged() should be(true)
          delta.minChanged() should be(false)
          delta.oldMin() should be(-2)
          delta.oldMax() should be(4)
          CPOutcome.Suspend
        }
        CPOutcome.Suspend
      }
    }

    val cp = CPSolver()
    val x = CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2 // -2,0,2,4
    cp.add(new MyCons(x))
    val cons = new LinkedList[Constraint]()
    cons.add(x < 4)
    cons.add(x < 2)
    cp.add(cons)
    // println("x dom:"+x.toSet)
    propag should be(true)
  }
  
  
  test("test delta 5 (with views)") {
    var propag = false
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MAXPRIORL2-5 
      override def setup(l: CPPropagStrength): CPOutcome = {
        X.filterWhenDomainChanges { delta =>
          propag = true
          delta.changed() should be(true)
          delta.size() should be(2)
          delta.values().toSet should be(Set(-2,-4))
          delta.maxChanged() should be(false)
          delta.minChanged() should be(true)
          delta.oldMin() should be(-4)
          delta.oldMax() should be(2)
          CPOutcome.Suspend
        }
        CPOutcome.Suspend
      }
    }

    val cp = CPSolver()
    val x = -(CPIntVar(Array(1, 3, 5, 7))(cp) -2 -3 + 2) // -4,-2,0,2
    cp.add(new MyCons(x))
    val cons = new LinkedList[Constraint]()
    cons.add(x > -4)
    cons.add(x > -2)
    cp.add(cons)
    //println("x dom:"+x.toSet)
    propag should be(true)
  }   
  
  
  
}
