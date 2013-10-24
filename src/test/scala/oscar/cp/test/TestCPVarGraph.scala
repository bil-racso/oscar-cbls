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
    val nnodes : Int = 4
    val edges = List((0,1),(1,2),(0,2))
    
    val graph = new CPVarGraph(cp, nnodes, edges)
    
    // nothing should be mandatory at start, all should be possible
    for (n <- 0 to (nnodes-1)){
      graph.mandatoryInEdges(n).isEmpty should be (true)
      graph.mandatoryOutEdges(n).isEmpty should be (true)
      graph.mandatoryEdges(n).isEmpty should be (true)
    }

    graph.mandatoryNodes.isEmpty should be (true)
    graph.possibleNodes should be ((0 to (nnodes-1)))
    
    // edges are : 0 = (0,1), 1 = (1,2), 2 = (0,2)
    graph.mandatoryInEdges(0).sorted  should be (List())
    graph.mandatoryOutEdges(0).sorted should be (List())
    graph.mandatoryEdges(0).sorted    should be (List())
    graph.possibleInEdges(0).sorted   should be (List())
    graph.possibleOutEdges(0).sorted  should be (List(0,2))
    graph.possibleEdges(0).sorted     should be (List(0,2))
    
    graph.mandatoryInEdges(1).sorted  should be (List())
    graph.mandatoryOutEdges(1).sorted should be (List())
    graph.mandatoryEdges(1).sorted    should be (List())
    graph.possibleInEdges(1).sorted   should be (List(0))
    graph.possibleOutEdges(1).sorted  should be (List(1))
    graph.possibleEdges(1).sorted     should be (List(0,1))
    
    graph.mandatoryInEdges(2).sorted  should be (List())
    graph.mandatoryOutEdges(2).sorted should be (List())
    graph.mandatoryEdges(2).sorted    should be (List())
    graph.possibleInEdges(2).sorted   should be (List(1,2))
    graph.possibleOutEdges(2).sorted  should be (List())
    graph.possibleEdges(2).sorted     should be (List(1,2))
    
    graph.mandatoryInEdges(3).sorted  should be (List())
    graph.mandatoryOutEdges(3).sorted should be (List())
    graph.mandatoryEdges(3).sorted    should be (List())
    graph.possibleInEdges(3).sorted   should be (List())
    graph.possibleOutEdges(3).sorted  should be (List())
    graph.possibleEdges(3).sorted     should be (List())
  }
}