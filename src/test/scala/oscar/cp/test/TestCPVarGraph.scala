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

  test("Test Set 1") {
    val cp = CPSolver()
    var nodes = new CPVarSet(cp, 0 , 2)
    var edges = new CPVarSet(cp, 1 , 2)
    cp.add(edges ++ 5)
    
    val graph = new CPVarGraph(cp, nodes, edges)
    /* according to definition, there are 3 nodes with id = 0 to 2
     * there are 3 arcs :
     * 		id = 1 -> (0,1); id = 2 -> (0,2); id = 5 -> (1,2) */
    
    graph.possibleEdges should be (Array(1,2,5))
    graph.possibleInEdges(0) should be(Array())
    graph.possibleOutEdges(0) should be(Array(1,2))
    
  }

}