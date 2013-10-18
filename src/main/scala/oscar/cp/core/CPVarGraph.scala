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
   * Gives mandatory outer edges from a node n
   * @param node
   * @return Return an array with the index of all mandatory outgoing edges
   */
  def mandatoryOutEdges(node : Int) : Array[Int] = Array()
  
  /**
   * Gives mandatory inner edges from a node n
   * @param node
   * @return Return an array with the index of all mandatory incoming edges
   */
  def mandatoryInEdges(node : Int) : Array[Int] = Array()
  
  /**
   * Gives possible outer edges from a node n
   * @param node
   * @return Return an array with the index of all possible outgoing edges
   */
  def possibleOutEdges(node : Int) : Array[Int] = Array()
  
  /**
   * Gives possible inner edges from a node n
   * @param node
   * @return Return an array with the index of all possible incoming edges
   */
  def possibleInEdges(node : Int) : Array[Int] = Array()

}