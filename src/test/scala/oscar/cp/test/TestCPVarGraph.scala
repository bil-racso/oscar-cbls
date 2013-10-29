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

  test("Test 1 : Getters") { // Test Getters of CPVarGraph
    val cp = CPSolver()
    val nnodes : Int = 4
    val edges = List((0,1),(1,2),(0,2),(5,6))
    
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
    // -> the edge (5,6) is not in range and should not be taken into account
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
  
  test("Test 2 : Node Setters") { // Test add/remove node based on lists
    /* we have 6 cases to test :
     *    we have 2 methods (addNode, removeNode)
     *       we have to check what happen if the node in arguments
     *       was in the 3 possible states : (required, possible, not possible)
     *    Notation : we will denoted cases from 1.1..1.3,2.1...2.3 
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
  }
  
  test("Test 3 : Outcome of node Setters") {
    val cp = CPSolver()
    val nnodes : Int = 4
    val edges = List((0,1),(1,2),(0,2))
    val nlist = List(0,1,2,3)
    
    val graph = new CPVarGraph(cp, nnodes, edges)
    // add node from possible nodes
    graph.addNode(0) should be (CPOutcome.Suspend)
    // add already required node
    graph.addNode(0) should be (CPOutcome.Suspend) 
    // remove a node that was required : Failure 
    graph.removeNode(0) should be (CPOutcome.Failure)
        
    // remove node from possible nodes
    graph.removeNode(2) should be (CPOutcome.Suspend) 
    // remove not possible node
    graph.removeNode(2) should be (CPOutcome.Suspend) 
    // add to required set a removed node : Failure
    graph.addNode(2) should be (CPOutcome.Failure)   
  }
  
  test("Test 4 : Edge Setters") { // Test add/remove edge
    /* we have 6 cases to test :
     *    we have 2 methods (addEdge, removeEdge)
     *       we have to check what happen if the edge in arguments
     *       was in the 3 possible states : (required, possible, not possible)
     *    Notation : we will denoted cases from 1.1..1.3,2.1...2.3 
     *       we will use this notation to be assured that we did not miss a test case
     */
    val cp = CPSolver()
    val nnodes : Int = 4
    val edges = List((0,1),(1,2),(0,2))
    val nlist = List(0,1,2,3)

    val graph = new CPVarGraph(cp, nnodes, edges)
    graph.possibleEdges(0).sorted    should be (List(0,2))
    graph.possibleInEdges(0).sorted  should be (List())
    graph.possibleOutEdges(0).sorted should be (List(0,2))
    graph.requiredEdges(0).sorted    should be (List())
    graph.requiredInEdges(0).sorted  should be (List())
    graph.requiredOutEdges(0).sorted should be (List())
    graph.possibleEdges(1).sorted    should be (List(0,1))
    graph.possibleInEdges(1).sorted  should be (List(0))
    graph.possibleOutEdges(1).sorted should be (List(1))
    graph.requiredEdges(1).sorted    should be (List())
    graph.requiredInEdges(1).sorted  should be (List())
    graph.requiredOutEdges(1).sorted should be (List())
    graph.requiredNodes.sorted       should be (List())
    graph.possibleNodes.sorted       should be (nlist)
    // add edge from possible edges  : case 1.2
    graph.addEdge(0, 1)
    // it add edge 0 : (0,1) to requiredEdges of 0 and 1 (out and in resp.)
    // and it set node 0 and 1 required
    graph.possibleEdges(0).sorted    should be (List(0,2))
    graph.possibleInEdges(0).sorted  should be (List())
    graph.possibleOutEdges(0).sorted should be (List(0,2))
    graph.requiredEdges(0).sorted    should be (List(0))
    graph.requiredInEdges(0).sorted  should be (List())
    graph.requiredOutEdges(0).sorted should be (List(0))
    graph.possibleEdges(1).sorted    should be (List(0,1))
    graph.possibleInEdges(1).sorted  should be (List(0))
    graph.possibleOutEdges(1).sorted should be (List(1))
    graph.requiredEdges(1).sorted    should be (List(0))
    graph.requiredInEdges(1).sorted  should be (List(0))
    graph.requiredOutEdges(1).sorted should be (List())
    graph.requiredNodes.sorted       should be (List(0,1))
    graph.possibleNodes.sorted       should be (nlist)
    // add already required edge, change nothing : case 1.1
    graph.addEdge(0, 1)
    graph.possibleEdges(0).sorted    should be (List(0,2))
    graph.possibleInEdges(0).sorted  should be (List())
    graph.possibleOutEdges(0).sorted should be (List(0,2))
    graph.requiredEdges(0).sorted    should be (List(0))
    graph.requiredInEdges(0).sorted  should be (List())
    graph.requiredOutEdges(0).sorted should be (List(0))
    graph.possibleEdges(1).sorted    should be (List(0,1))
    graph.possibleInEdges(1).sorted  should be (List(0))
    graph.possibleOutEdges(1).sorted should be (List(1))
    graph.requiredEdges(1).sorted    should be (List(0))
    graph.requiredInEdges(1).sorted  should be (List(0))
    graph.requiredOutEdges(1).sorted should be (List())
    graph.requiredNodes.sorted       should be (List(0,1))
    graph.possibleNodes.sorted       should be (nlist)
    // remove required edge : error
    // should change nothing :   case 2.1
    graph.removeEdge(0, 1)
    graph.possibleEdges(0).sorted    should be (List(0,2))
    graph.possibleInEdges(0).sorted  should be (List())
    graph.possibleOutEdges(0).sorted should be (List(0,2))
    graph.requiredEdges(0).sorted    should be (List(0))
    graph.requiredInEdges(0).sorted  should be (List())
    graph.requiredOutEdges(0).sorted should be (List(0))
    graph.possibleEdges(1).sorted    should be (List(0,1))
    graph.possibleInEdges(1).sorted  should be (List(0))
    graph.possibleOutEdges(1).sorted should be (List(1))
    graph.requiredEdges(1).sorted    should be (List(0))
    graph.requiredInEdges(1).sorted  should be (List(0))
    graph.requiredOutEdges(1).sorted should be (List())
    graph.requiredNodes.sorted       should be (List(0,1))
    graph.possibleNodes.sorted       should be (nlist)
    
    val graph2 = new CPVarGraph(cp, nnodes, edges)
    graph2.possibleEdges(0).sorted    should be (List(0,2))
    graph2.possibleInEdges(0).sorted  should be (List())
    graph2.possibleOutEdges(0).sorted should be (List(0,2))
    graph2.requiredEdges(0).sorted    should be (List())
    graph2.requiredInEdges(0).sorted  should be (List())
    graph2.requiredOutEdges(0).sorted should be (List())
    graph2.possibleEdges(1).sorted    should be (List(0,1))
    graph2.possibleInEdges(1).sorted  should be (List(0))
    graph2.possibleOutEdges(1).sorted should be (List(1))
    graph2.requiredEdges(1).sorted    should be (List())
    graph2.requiredInEdges(1).sorted  should be (List())
    graph2.requiredOutEdges(1).sorted should be (List())
    graph2.requiredNodes.sorted       should be (List())
    graph2.possibleNodes.sorted       should be (nlist)
    // remove edge from possible edges  : case 2.2
    graph2.removeEdge(0, 1)
    // it remove edge 0 : (0,1) from possibleEdges of 0 and 1 
    //    (out and in resp.)
    graph2.possibleEdges(0).sorted    should be (List(2))
    graph2.possibleInEdges(0).sorted  should be (List())
    graph2.possibleOutEdges(0).sorted should be (List(2))
    graph2.requiredEdges(0).sorted    should be (List())
    graph2.requiredInEdges(0).sorted  should be (List())
    graph2.requiredOutEdges(0).sorted should be (List())
    graph2.possibleEdges(1).sorted    should be (List(1))
    graph2.possibleInEdges(1).sorted  should be (List())
    graph2.possibleOutEdges(1).sorted should be (List(1))
    graph2.requiredEdges(1).sorted    should be (List())
    graph2.requiredInEdges(1).sorted  should be (List())
    graph2.requiredOutEdges(1).sorted should be (List())
    graph2.requiredNodes.sorted       should be (List())
    graph2.possibleNodes.sorted       should be (nlist)
    // remove already removed edge, change nothing : case 2.3
    graph2.removeEdge(0,1)
    graph2.possibleEdges(0).sorted    should be (List(2))
    graph2.possibleInEdges(0).sorted  should be (List())
    graph2.possibleOutEdges(0).sorted should be (List(2))
    graph2.requiredEdges(0).sorted    should be (List())
    graph2.requiredInEdges(0).sorted  should be (List())
    graph2.requiredOutEdges(0).sorted should be (List())
    graph2.possibleEdges(1).sorted    should be (List(1))
    graph2.possibleInEdges(1).sorted  should be (List())
    graph2.possibleOutEdges(1).sorted should be (List(1))
    graph2.requiredEdges(1).sorted    should be (List())
    graph2.requiredInEdges(1).sorted  should be (List())
    graph2.requiredOutEdges(1).sorted should be (List())
    graph2.requiredNodes.sorted       should be (List())
    graph2.possibleNodes.sorted       should be (nlist)
    // add edge that was not possible : 
    //    error -> should change nothing :   case 1.1
    graph2.addEdge(0,1)
    graph2.possibleEdges(0).sorted    should be (List(2))
    graph2.possibleInEdges(0).sorted  should be (List())
    graph2.possibleOutEdges(0).sorted should be (List(2))
    graph2.requiredEdges(0).sorted    should be (List())
    graph2.requiredInEdges(0).sorted  should be (List())
    graph2.requiredOutEdges(0).sorted should be (List())
    graph2.possibleEdges(1).sorted    should be (List(1))
    graph2.possibleInEdges(1).sorted  should be (List())
    graph2.possibleOutEdges(1).sorted should be (List(1))
    graph2.requiredEdges(1).sorted    should be (List())
    graph2.requiredInEdges(1).sorted  should be (List())
    graph2.requiredOutEdges(1).sorted should be (List())
    graph2.requiredNodes.sorted       should be (List())
    graph2.possibleNodes.sorted       should be (nlist)
  }
  
  test("Test 5 : Test Outcome of edge Setters") {
    val cp = CPSolver()
    val nnodes : Int = 4
    val edges = List((0,1),(1,2),(0,2))
    val nlist = List(0,1,2,3)

    val graph = new CPVarGraph(cp, nnodes, edges)
    
    // add edge from possible edges to required set
    graph.addEdge(0, 1) should be (CPOutcome.Suspend)
    // add already required edge
    graph.addEdge(0, 1) should be (CPOutcome.Suspend)
    // remove required edge : error
    graph.removeEdge(0, 1) should be (CPOutcome.Failure)
     
    // remove edge from possible edges
    graph.removeEdge(1, 2) should be (CPOutcome.Suspend)
    // remove already removed edge
    graph.removeEdge(1, 2) should be (CPOutcome.Suspend) 
    // add edge that was not possible
    graph.addEdge(1, 2) should be (CPOutcome.Failure)
  }
  
  test("Test 6 : Add out of range nodes & edges") {
    val cp = CPSolver()
    val nnodes : Int = 4
    val edges = List((0,1),(1,2),(0,2))
    val nlist = List(0,1,2,3)
    
    val graph = new CPVarGraph(cp, nnodes, edges)
    
    // add node not in interval/range : 
    //   should fail and change nothing to lists
    graph.addNode(5)  should be (CPOutcome.Failure)
    graph.addNode(-1) should be (CPOutcome.Failure)
    graph.requiredNodes.sorted       should be (List())
    graph.possibleNodes.sorted       should be (nlist)
    // add edge not in interval or range :
    //     should fail and change nothing in lists
    graph.addEdge(2,3)  should be (CPOutcome.Failure)
    graph.addEdge(2,6)  should be (CPOutcome.Failure)
    graph.addEdge(-2,0) should be (CPOutcome.Failure)
    // should change nothing to list of nodes and edges
    graph.possibleEdges(0).sorted    should be (List(0,2))
    graph.possibleInEdges(0).sorted  should be (List())
    graph.possibleOutEdges(0).sorted should be (List(0,2))
    graph.requiredEdges(0).sorted    should be (List())
    graph.requiredInEdges(0).sorted  should be (List())
    graph.requiredOutEdges(0).sorted should be (List())
    graph.possibleEdges(1).sorted    should be (List(0,1))
    graph.possibleInEdges(1).sorted  should be (List(0))
    graph.possibleOutEdges(1).sorted should be (List(1))
    graph.requiredEdges(1).sorted    should be (List())
    graph.requiredInEdges(1).sorted  should be (List())
    graph.requiredOutEdges(1).sorted should be (List())
    graph.possibleEdges(2).sorted    should be (List(1,2))
    graph.possibleInEdges(2).sorted  should be (List(1,2))
    graph.possibleOutEdges(2).sorted should be (List())
    graph.requiredEdges(2).sorted    should be (List())
    graph.requiredInEdges(2).sorted  should be (List())
    graph.requiredOutEdges(2).sorted should be (List())
    graph.requiredNodes.sorted       should be (List())
    graph.possibleNodes.sorted       should be (nlist)
  } 
}