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

  test("test delta 6: queens") {

    class Cons1(val x: Array[CPIntVar]) extends Constraint(x(0).store, "Cons1") {

      val delta1 = Array.tabulate(x.size)(i => Array.ofDim[Int](x(i).size))
      val delta2 = Array.tabulate(x.size)(i => Array.ofDim[Int](x(i).size))
      var currDim = Array.tabulate(x.size)(i => 0)

      override def setup(l: CPPropagStrength): CPOutcome = {

        for (i <- 0 until x.size) {
          x(i).callPropagateWhenDomainChanges(this, true)
          x(i).callValRemoveIdxWhenValueIsRemoved(this, i)
        }
        propagate()

      }

      s.onPop {
        for (i <- 0 until x.size) {
          currDim(i) = 0
        }
      }

      override def valRemoveIdx(x: CPIntVar, idx: Int, value: Int) = {
        delta1(idx)(currDim(idx)) = value
        currDim(idx) += 1
        CPOutcome.Suspend
      }

      override def propagate(): CPOutcome = {
        for (i <- 0 until x.size) {

          val m = x(i).fillDeltaArray(this, delta2(i))
          val s1 = delta2(i).take(m)
          val s2 = delta1(i).take(currDim(i))
          val s3 = x(i).delta(this).toArray
          assert(m == currDim(i))
          
          assert(s1.length == s2.length)
          assert(s1.length == s3.length)
          assert(s1.toSet == s2.toSet)
          assert(s1.toSet == s3.toSet)          
          
        }

        for (i <- 0 until x.size) {
          currDim(i) = 0
        }
        CPOutcome.Suspend
      }
    }

    implicit val cp = CPSolver()
    cp.silent = true
    val n = 11 //number of queens
    val Queens = 0 until n
    //variables
    val queens = for (i <- Queens) yield CPIntVar(cp, 1 to n)

    var nbsol = 0

    cp.add(new Cons1(queens))
    cp.add(new Cons1(for (i <- Queens) yield queens(i) + i))
    cp.add(new Cons1(for (i <- Queens) yield queens(i) - i))

    cp.add(allDifferent(queens), Strong)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) + i), Strong)
    cp.add(allDifferent(for (i <- Queens) yield queens(i) - i), Strong)

    // Search heuristic
    search(binaryFirstFail(queens,_.randomValue))

    // Execution
    val stats = start()
    println(stats)

  }
  
  
  test("test delta 7") {
    var nPropagates = 0
    
    class MyCons(val X: CPIntVar) extends Constraint(X.store, "TestDelta") {
      priorityL2 = CPStore.MAXPRIORL2-5 
      override def setup(l: CPPropagStrength): CPOutcome = {
        X.callPropagateWhenDomainChanges(this, true)
        // I remove some values such that propagate should be called again
        X.updateMax(5)
        X.updateMin(1)
        X.removeValue(3)
        CPOutcome.Suspend
      }
      override def propagate(): CPOutcome = {
        nPropagates += 1
        assert(X.delta(this).toSet == Set(0,3,6,7,8)) 
        val array = Array.ofDim[Int](6)
        val m = X.fillDeltaArray(this, array)
        assert(m == 5)
        assert(array.take(m).toSet == Set(0,3,6,7,8))
        CPOutcome.Suspend
      }
    }
    
  

    val cp = CPSolver()
    val x = (CPIntVar(0 to 8)(cp) -2 + 2)
    cp.add(new MyCons(x))

    //println("x dom:"+x.toSet)
    assert(nPropagates == 1)
  }  

}
