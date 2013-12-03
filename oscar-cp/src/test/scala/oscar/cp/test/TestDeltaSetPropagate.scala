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
import collection.immutable.SortedSet
import java.util.LinkedList


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestDeltaSetPropagate extends FunSuite with ShouldMatchers {
  



  test("test delta set 1") {
    var propag = false
    
    class MyCons(val X: CPVarSet) extends Constraint(X.s, "TestDelta") {
      priorityL2 = CPStore.MAXPRIORL2-5 
      override def setup(l: CPPropagStrength): CPOutcome = {
        println("setup")
        X.filterWhenDomainChanges { delta =>
          propag = true
          
          delta.changed should be(true)
          delta.requiredChanged should be(true)
          delta.possibleChanged should be(true)
          delta.deltaRequiredSize should be(2)
          delta.deltaRequired.toSet should be(Set(1,3))
          delta.deltaPossibleSize should be(2)
          delta.deltaPossible.toSet should be(Set(2,4))
          

          CPOutcome.Suspend
        }
        CPOutcome.Suspend
      }
    }

    val cp = CPSolver()
    val x = new CPVarSet(cp, 1 , 5)
    println(x.requiredSize+" "+x.possibleSize)
    cp.add(new MyCons(x))
    
    val cons = new LinkedList[Constraint]()
    cons.add(x ++ 1)
    cons.add(x ++ 3)
    cons.add(x -- 2)
    cons.add(x -- 4)
    
    cp.add(cons)
    
    println(x.requiredSet+" <= x <="+x.possibleSet)
    propag should be(true)
  } 
  
  
  
}
