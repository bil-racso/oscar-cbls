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

import oscar.cp.core._

import oscar.cp.modeling._

/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

class TestCPVarGraph extends FunSuite with ShouldMatchers  {

  test("Test Set 1") { // Test Getters of CPVarGraph
    val cp = CPSolver()
    val nnodes : Int = 3
    val edges = List((0,1),(1,2),(2,0))
    
    val graph = new CPVarGraph(cp, nnodes, edges)
    
    // nothing should be mandatory at start, all should be possible
    for (n <- 0 to nnodes ){
      graph.mandatoryInEdges(n).isEmpty should be (true)
      graph.mandatoryOutEdges(n).isEmpty should be (true)
      graph.mandatoryEdges(n).isEmpty should be (true)
    }
    graph.mandatoryNodes.isEmpty should be (true)
    
    // edges are : 0 = (0,1), 1 = (1,2), 2 = (0,2)
    graph.possibleNodes should be ((0 to nnodes).toArray)
    graph.possibleEdges(0) 		should be (Array(0,2))
    graph.possibleInEdges(0) 	should be (Array())
    graph.possibleOutEdges(0) 	should be (Array(0,2))
    graph.possibleEdges(1) 		should be (Array(0,1))
    graph.possibleInEdges(1) 	should be (Array(0))
    graph.possibleOutEdges(1) 	should be (Array(1))
    graph.possibleEdges(2) 		should be (Array(1,2))
    graph.possibleInEdges(2) 	should be (Array(1,2))
    graph.possibleOutEdges(2) 	should be (Array())
    graph.possibleEdges(3) 		should be (Array())
    graph.possibleInEdges(3) 	should be (Array())
    graph.possibleOutEdges(3) 	should be (Array())
    
  }

}