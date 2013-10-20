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
package oscar.cp.core


/**
 * @author Andrew Lambert andrew.lambert@student.uclouvain.be
 */

/**
 * Build a graph CPVar
 * @param 	nNodes : number of nodes of the maximal graph
 * 			inputEdges : list of tuple/pair (source, destination) representing the edges of the maximal graph
 */
class CPVarGraph(val s: CPStore, nNodes: Int, inputEdges: List[(Int,Int)], val name: String = "") extends CPVar {
  
  def store = s 
  val nedges = inputEdges.length
  
  // N and E will hold current graph interval
  private val N = new CPVarSet(store,0,nNodes)
  private val E = new CPVarSet(store,0,nedges)
  
  // define some useful inner class
  // Edge() take 3 param to have immutable src and destination
  class Edge(index : Int, source : Int, destination : Int){
    val i = index
    val src = source
    val dest= destination
  }
  class Node(index : Int){
    val i = index
    var inEdges  : List[Int] = Nil
    var outEdges : List[Int] = Nil
  }
  
  // the following structures will be used to access easily to the hidden graph
  val nodes : Array[Node] = Array.tabulate(nNodes)(i => new Node(i))
  val edges : Array[Edge] = Array.tabulate(nedges)(i => new Edge(i,inputEdges(i)._1,inputEdges(i)._2 ) )
  // fill array nodes with inEdges and outEdges
  for(i <- 0 to nedges){
    val e = edges(i)
    e.i :: nodes(e.src ).inEdges
    e.i :: nodes(e.dest).outEdges
  }
  
   /**
   * @param node Id
   * @return Return an array with the index of all mandatory outgoing edges from the node
   */
  def mandatoryOutEdges(nodeId: Int) : Array[Int] = nodes(nodeId).outEdges.filter( E isRequired _ ).toArray
  
  /**
   * @param node Id
   * @return Return an array with the index of all mandatory incoming edges from the node
   */
  def mandatoryInEdges(nodeId: Int) : Array[Int] = nodes(nodeId).inEdges.filter( E isRequired _ ).toArray

  /**
   * @param node Id
   * @return Return an array with the index of all mandatory edges from the node
   */
  def mandatoryEdges(nodeId: Int) : Array[Int] = mandatoryInEdges(nodeId) ++ mandatoryOutEdges(nodeId)
  
  /**
   * @param node Id
   * @return Return an array with the index of all possible outgoing edges from the node
   */
  def possibleOutEdges(nodeId: Int) : Array[Int] = nodes(nodeId).outEdges.filter( E isPossible _ ).toArray
  
  /**
   * @param node Id
   * @return Return an array with the index of all possible incoming edgesfrom the node
   */
  def possibleInEdges(nodeId: Int) : Array[Int] = nodes(nodeId).inEdges.filter( E isPossible _ ).toArray

  /**
   * @param node Id
   * @return Return an array with the index of all possible edges from the node
   */
  def possibleEdges(nodeId: Int) : Array[Int] = possibleInEdges(nodeId) ++ possibleOutEdges(nodeId)
  
  /* TODO : HANDLE ERROR CASES IN ADD/REMOVE FUNCTIONS */
  
  /**
   * Add a node into mandatory nodes from graph interval
   * @param node Id
   */
  def addMandatoryNode(nodeId: Int) : CPOutcome = N.requires(nodeId)
  
  /**
   * Remove a node from possible nodes from graph interval
   * @param node Id
   */
  def removeNode(nodeId: Int) : CPOutcome = N.excludes(nodeId)
  
  /**
   * Add an edge into mandatory edges from graph interval
   * @param source and destination of the edge
   */
  def addMandatoryEdge(src: Int, dest: Int) : CPOutcome = {
    val index = indexOfEdge(src, dest)
    if (index == -1) CPOutcome.Failure
    else E.requires(index)
  }
  
  /**
   * Remove an edge from possible edges from graph interval
   * @param source and destination of the edge
   */
  def removeEdge(src: Int, dest: Int) : CPOutcome = {
    val index = indexOfEdge(src, dest)
    if (index == -1) CPOutcome.Failure
    else E.excludes(index)
  }
  
  /**
   * @return the index of the edge (src,dest)
   * 			-1 if the edge is not in the graph interval
   */
  private def indexOfEdge(src: Int, dest: Int) : Int = {
    val edge = edges.filter(e => (e.src == src && e.dest == dest))
    if (edge.isEmpty) -1
    else edge(0).i
  }
  
}

