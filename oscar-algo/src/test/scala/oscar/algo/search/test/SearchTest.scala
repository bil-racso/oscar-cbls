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
package oscar.algo.search.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.algo.search._
import oscar.algo.reversible._
import oscar.algo.search.SearchNode

class SearchTest extends FunSuite with ShouldMatchers  {

  test("test search1") {
    val node = new SearchNode()

    def branch(left: => Unit)(right: => Unit) = Seq(() => left, () => right)

    //def search
    val b = new Branching() {
      val i = new ReversibleInt(node, 0)

      override def alternatives = {
        if (i.value > 2) noAlternative
        else {
          branch {i.incr() } { i.incr() }
        }
      }
    }
    
    var nbSol = 0
    val search = new Search(node, b)
    search.onSolution {
      nbSol +=1 
    }
    
    nbSol = 0
    search.solveAll().nbSols should be(8)
    nbSol should be(8)
    
    nbSol = 0
    search.solveAll(nbSols = 2).nbSols should be(2)
    nbSol should be(2)
    
    nbSol = 0
    search.solveAll(failureLimit = 5).nbSols should be(5)
    nbSol should be(5)
    
    nbSol = 0
    search.solveAll(maxDiscrepancy = 0).nbSols should be(1)
    nbSol should be(1)
    
    nbSol = 0
    search.solveAll(maxDiscrepancy = 1).nbSols should be(4)  
    nbSol should be(4)
    
    nbSol = 0
    search.solveAll(maxDiscrepancy = 2).nbSols should be(7)  
    nbSol should be(7)    

    nbSol = 0
    search.solveAll(maxDiscrepancy = 3).nbSols should be(8)  
    nbSol should be(8)     
  }
  
  test("test search2") {
    val node = new SearchNode()

    val d = Array(false, false, false)

    def branch(left: => Unit)(right: => Unit) = Seq(() => left, () => right)

    //def search
    val b = new Branching() {
      val i = new ReversibleInt(node, 0)

      override def alternatives = {
        if (i.value >= d.size) noAlternative
        else {
          branch {
            d(i.value) = true
            i.incr()
            if (d.count(v => v) == 2) node.fail() // if two values = true, we fail
          } {
            d(i.value) = false
            i.incr()
            if (d.count(v => v) == 2) node.fail() // if two values = true, we fail
          }
        }
      }
    }
    // solutions
    
    var nbSol = 0
    val search = new Search(node, b)
    search.onSolution {
      nbSol +=1 
    }
    nbSol = 0
    search.solveAll().nbSols should be(4)
    nbSol should be(4)
    
    nbSol = 0
    search.solveAll(failureLimit = 1).nbSols should be(0)
  }
  
  
  test("test search3") {
    val node = new SearchNode()

    def branch(left: => Unit)(right: => Unit) = Seq(() => left, () => right)

    //def search
    val b = new Branching() {
      val i = new ReversibleInt(node, 0)

      override def alternatives = {
        if (i.value > 30) noAlternative
        else {
          branch {i.incr() } { i.incr() }
        }
      }
    }
    
    val search = new Search(node, b)
     
    search.solveAll(timeLimit = 1).time should be <= 15000L
    
  }
    


}

