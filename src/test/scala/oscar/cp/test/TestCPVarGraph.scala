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
    
    // nothing should be required at start, all should be possible
    for (n <- 0 to (nnodes-1)){
      graph.requiredInEdges(n).isEmpty should be (true)
      graph.requiredOutEdges(n).isEmpty should be (true)
      graph.requiredEdges(n).isEmpty should be (true)
    }

    graph.requiredNodes.isEmpty should be (true)
    graph.possibleNodes         should be ((0 to (nnodes-1)))
    
    // edges are : 0 = (0,1), 1 = (1,2), 2 = (0,2)
    graph.requiredInEdges(0).sorted  should be (List())
    graph.requiredOutEdges(0).sorted should be (List())
    graph.requiredEdges(0).sorted    should be (List())
    graph.possibleInEdges(0).sorted  should be (List())
    graph.possibleOutEdges(0).sorted should be (List(0,2))
    graph.possibleEdges(0).sorted    should be (List(0,2))
    
    graph.requiredInEdges(1).sorted  should be (List())
    graph.requiredOutEdges(1).sorted should be (List())
    graph.requiredEdges(1).sorted    should be (List())
    graph.possibleInEdges(1).sorted  should be (List(0))
    graph.possibleOutEdges(1).sorted should be (List(1))
    graph.possibleEdges(1).sorted    should be (List(0,1))
    
    graph.requiredInEdges(2).sorted  should be (List())
    graph.requiredOutEdges(2).sorted should be (List())
    graph.requiredEdges(2).sorted    should be (List())
    graph.possibleInEdges(2).sorted  should be (List(1,2))
    graph.possibleOutEdges(2).sorted should be (List())
    graph.possibleEdges(2).sorted    should be (List(1,2))
    
    graph.requiredInEdges(3).sorted  should be (List())
    graph.requiredOutEdges(3).sorted should be (List())
    graph.requiredEdges(3).sorted    should be (List())
    graph.possibleInEdges(3).sorted  should be (List())
    graph.possibleOutEdges(3).sorted should be (List())
    graph.possibleEdges(3).sorted    should be (List())
  }
  
  test("Test Set 2") { // Test add/remove node/edges
    /* we have 12 cases to test :
     *    we have 4 methods (addNode, removeNode, addEdge, removeEdge)
     *       we have to check what happen if the node/edge in arguments
     *       was in the 3 possible states : (required, possible, not possible)
     *    Notation : we will denoted cases from 1.1..1.3,2.1,...4.3 
     *       we will use this notation to be assured that we did not miss a test case
     */
    val cp = CPSolver()
    val nnodes : Int = 4
    val edges = List((0,1),(1,2),(0,2))
    val nlist = List(0,1,2,3)
    
    val graph = new CPVarGraph(cp, nnodes, edges)
    graph.requiredNodes.sorted should be (List())
    graph.possibleNodes.sorted should be (nlist)
    // add node from possible nodes : case 1.2
    graph.addNode(0)
    graph.requiredNodes.sorted should be (List(0))
    graph.possibleNodes.sorted should be (nlist)
    // add already required node : do nothing : case 1.1
    graph.addNode(0) 
    graph.requiredNodes.sorted should be (List(0))
    graph.possibleNodes.sorted should be (nlist)
    // remove a node that was required : 
    //    error -> should change nothing  : case 2.1
    graph.removeNode(0)
    graph.requiredNodes.sorted should be (List(0))
    graph.possibleNodes.sorted should be (nlist)
    
    val graph2 = new CPVarGraph(cp, nnodes, edges)
    graph2.requiredNodes.sorted should be (List())
    graph2.possibleNodes.sorted should be (nlist)
    graph2.possibleEdges(0).sorted    should be (List(0,2))
    graph2.possibleInEdges(0).sorted  should be (List())
    graph2.possibleOutEdges(0).sorted should be (List(0,2))
    graph2.requiredEdges(0).sorted    should be (List())
    graph2.requiredInEdges(0).sorted  should be (List())
    graph2.requiredOutEdges(0).sorted should be (List())
    // remove node from possible nodes : case 2.2
    // also remove connected edges to node 0
    //   -> in this case, edges 0:(0,1) and 2:(0,2)
    graph2.removeNode(0) 
    graph2.requiredNodes.sorted should be (List())
    graph2.possibleNodes.sorted should be (List(1,2,3))
    graph2.possibleEdges(0).sorted    should be (List())
    graph2.possibleInEdges(0).sorted  should be (List())
    graph2.possibleOutEdges(0).sorted should be (List())
    graph2.requiredEdges(0).sorted    should be (List())
    graph2.requiredInEdges(0).sorted  should be (List())
    graph2.requiredOutEdges(0).sorted should be (List())
    // remove not possible node : do nothing : case 2.3
    graph2.removeNode(0) 
    graph2.requiredNodes.sorted should be (List())
    graph2.possibleNodes.sorted should be (List(1,2,3))
    // add to required set a removed node :
    //    error -> should change nothing  : case 1.3
    graph2.addNode(0) 
    graph2.requiredNodes.sorted should be (List())
    graph2.possibleNodes.sorted should be (List(1,2,3))
    
    
    // we will now check edges functions, 
    // to remember, val edges = List((0,1),(1,2),(0,2))
    val graph3 = new CPVarGraph(cp, nnodes, edges)
    graph3.possibleEdges(0).sorted    should be (List(0,2))
    graph3.possibleInEdges(0).sorted  should be (List())
    graph3.possibleOutEdges(0).sorted should be (List(0,2))
    graph3.requiredEdges(0).sorted    should be (List())
    graph3.requiredInEdges(0).sorted  should be (List())
    graph3.requiredOutEdges(0).sorted should be (List())
    graph3.possibleEdges(1).sorted    should be (List(0,1))
    graph3.possibleInEdges(1).sorted  should be (List(0))
    graph3.possibleOutEdges(1).sorted should be (List(1))
    graph3.requiredEdges(1).sorted    should be (List())
    graph3.requiredInEdges(1).sorted  should be (List())
    graph3.requiredOutEdges(1).sorted should be (List())
    graph3.requiredNodes.sorted       should be (List())
    graph3.possibleNodes.sorted       should be (nlist)
    // add edge from possible edges  : case 3.2
    graph3.addEdge(0, 1)
    // it add edge 0 : (0,1) to requiredEdges of 0 and 1 (out and in resp.)
    // and it set node 0 and 1 required
    graph3.possibleEdges(0).sorted    should be (List(0,2))
    graph3.possibleInEdges(0).sorted  should be (List())
    graph3.possibleOutEdges(0).sorted should be (List(0,2))
    graph3.requiredEdges(0).sorted    should be (List(0))
    graph3.requiredInEdges(0).sorted  should be (List())
    graph3.requiredOutEdges(0).sorted should be (List(0))
    graph3.possibleEdges(1).sorted    should be (List(0,1))
    graph3.possibleInEdges(1).sorted  should be (List(0))
    graph3.possibleOutEdges(1).sorted should be (List(1))
    graph3.requiredEdges(1).sorted    should be (List(0))
    graph3.requiredInEdges(1).sorted  should be (List(0))
    graph3.requiredOutEdges(1).sorted should be (List())
    graph3.requiredNodes.sorted       should be (List(0,1))
    graph3.possibleNodes.sorted       should be (nlist)
    // add already required edge, change nothing : case 3.1
    graph3.addEdge(0, 1)
    graph3.possibleEdges(0).sorted    should be (List(0,2))
    graph3.possibleInEdges(0).sorted  should be (List())
    graph3.possibleOutEdges(0).sorted should be (List(0,2))
    graph3.requiredEdges(0).sorted    should be (List(0))
    graph3.requiredInEdges(0).sorted  should be (List())
    graph3.requiredOutEdges(0).sorted should be (List(0))
    graph3.possibleEdges(1).sorted    should be (List(0,1))
    graph3.possibleInEdges(1).sorted  should be (List(0))
    graph3.possibleOutEdges(1).sorted should be (List(1))
    graph3.requiredEdges(1).sorted    should be (List(0))
    graph3.requiredInEdges(1).sorted  should be (List(0))
    graph3.requiredOutEdges(1).sorted should be (List())
    graph3.requiredNodes.sorted       should be (List(0,1))
    graph3.possibleNodes.sorted       should be (nlist)
    // remove required edge : error
    // should change nothing :   case 4.1
    graph3.removeEdge(0, 1)
    graph3.possibleEdges(0).sorted    should be (List(0,2))
    graph3.possibleInEdges(0).sorted  should be (List())
    graph3.possibleOutEdges(0).sorted should be (List(0,2))
    graph3.requiredEdges(0).sorted    should be (List(0))
    graph3.requiredInEdges(0).sorted  should be (List())
    graph3.requiredOutEdges(0).sorted should be (List(0))
    graph3.possibleEdges(1).sorted    should be (List(0,1))
    graph3.possibleInEdges(1).sorted  should be (List(0))
    graph3.possibleOutEdges(1).sorted should be (List(1))
    graph3.requiredEdges(1).sorted    should be (List(0))
    graph3.requiredInEdges(1).sorted  should be (List(0))
    graph3.requiredOutEdges(1).sorted should be (List())
    graph3.requiredNodes.sorted       should be (List(0,1))
    graph3.possibleNodes.sorted       should be (nlist)
    
    val graph4 = new CPVarGraph(cp, nnodes, edges)
    graph4.possibleEdges(0).sorted    should be (List(0,2))
    graph4.possibleInEdges(0).sorted  should be (List())
    graph4.possibleOutEdges(0).sorted should be (List(0,2))
    graph4.requiredEdges(0).sorted    should be (List())
    graph4.requiredInEdges(0).sorted  should be (List())
    graph4.requiredOutEdges(0).sorted should be (List())
    graph4.possibleEdges(1).sorted    should be (List(0,1))
    graph4.possibleInEdges(1).sorted  should be (List(0))
    graph4.possibleOutEdges(1).sorted should be (List(1))
    graph4.requiredEdges(1).sorted    should be (List())
    graph4.requiredInEdges(1).sorted  should be (List())
    graph4.requiredOutEdges(1).sorted should be (List())
    graph4.requiredNodes.sorted       should be (List())
    graph4.possibleNodes.sorted       should be (nlist)
    // remove edge from possible edges  : case 4.2
    graph4.removeEdge(0, 1)
    // it remove edge 0 : (0,1) from possibleEdges of 0 and 1 
    //    (out and in resp.)
    graph4.possibleEdges(0).sorted    should be (List(2))
    graph4.possibleInEdges(0).sorted  should be (List())
    graph4.possibleOutEdges(0).sorted should be (List(2))
    graph4.requiredEdges(0).sorted    should be (List())
    graph4.requiredInEdges(0).sorted  should be (List())
    graph4.requiredOutEdges(0).sorted should be (List())
    graph4.possibleEdges(1).sorted    should be (List(1))
    graph4.possibleInEdges(1).sorted  should be (List())
    graph4.possibleOutEdges(1).sorted should be (List(1))
    graph4.requiredEdges(1).sorted    should be (List())
    graph4.requiredInEdges(1).sorted  should be (List())
    graph4.requiredOutEdges(1).sorted should be (List())
    graph4.requiredNodes.sorted       should be (List())
    graph4.possibleNodes.sorted       should be (nlist)
    // remove already removed edge, change nothing : case 4.3
    graph4.removeEdge(0,1)
    graph4.possibleEdges(0).sorted    should be (List(2))
    graph4.possibleInEdges(0).sorted  should be (List())
    graph4.possibleOutEdges(0).sorted should be (List(2))
    graph4.requiredEdges(0).sorted    should be (List())
    graph4.requiredInEdges(0).sorted  should be (List())
    graph4.requiredOutEdges(0).sorted should be (List())
    graph4.possibleEdges(1).sorted    should be (List(1))
    graph4.possibleInEdges(1).sorted  should be (List())
    graph4.possibleOutEdges(1).sorted should be (List(1))
    graph4.requiredEdges(1).sorted    should be (List())
    graph4.requiredInEdges(1).sorted  should be (List())
    graph4.requiredOutEdges(1).sorted should be (List())
    graph4.requiredNodes.sorted       should be (List())
    graph4.possibleNodes.sorted       should be (nlist)
    // add edge that was not possible : 
    //    error -> should change nothing :   case 3.1
    graph4.addEdge(0,1)
    graph4.possibleEdges(0).sorted    should be (List(2))
    graph4.possibleInEdges(0).sorted  should be (List())
    graph4.possibleOutEdges(0).sorted should be (List(2))
    graph4.requiredEdges(0).sorted    should be (List())
    graph4.requiredInEdges(0).sorted  should be (List())
    graph4.requiredOutEdges(0).sorted should be (List())
    graph4.possibleEdges(1).sorted    should be (List(1))
    graph4.possibleInEdges(1).sorted  should be (List())
    graph4.possibleOutEdges(1).sorted should be (List(1))
    graph4.requiredEdges(1).sorted    should be (List())
    graph4.requiredInEdges(1).sorted  should be (List())
    graph4.requiredOutEdges(1).sorted should be (List())
    graph4.requiredNodes.sorted       should be (List())
    graph4.possibleNodes.sorted       should be (nlist)
    

    // add node not in interval/range : 
    //   should fail and change nothing
    graph4.addNode(5)
    graph4.requiredNodes.sorted       should be (List())
    graph4.possibleNodes.sorted       should be (nlist)
    // add edge not in interval or range :
    //     should fail and change nothing
    graph4.addEdge(2,3)
    graph4.addEdge(2,6)
    graph4.possibleEdges(0).sorted    should be (List(2))
    graph4.possibleInEdges(0).sorted  should be (List())
    graph4.possibleOutEdges(0).sorted should be (List(2))
    graph4.requiredEdges(0).sorted    should be (List())
    graph4.requiredInEdges(0).sorted  should be (List())
    graph4.requiredOutEdges(0).sorted should be (List())
    graph4.possibleEdges(1).sorted    should be (List(1))
    graph4.possibleInEdges(1).sorted  should be (List())
    graph4.possibleOutEdges(1).sorted should be (List(1))
    graph4.requiredEdges(1).sorted    should be (List())
    graph4.requiredInEdges(1).sorted  should be (List())
    graph4.requiredOutEdges(1).sorted should be (List())
    graph4.requiredNodes.sorted       should be (List())
    graph4.possibleNodes.sorted       should be (nlist)
  }
}