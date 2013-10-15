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
 * @param 	nodes : nodes of the graph interval
 * 			edges : edges of the graph interval
 */
class CPVarGraph(val s: CPStore, nodes: CPVarSet, edges: CPVarSet, val name: String = "") extends CPVar {
  
  def store = s
  
  val order = nodes.card
  
  val possibleNodes : Array[Int] = nodes.possibleNotRequiredValues.toArray
  val possibleEdges : Array[Int] = edges.possibleNotRequiredValues.toArray
  
  val requiredNodes : Array[Int] = nodes.requiredValues.toArray
  val requiredEdges : Array[Int] = edges.requiredValues.toArray
  
  
  /* TODO implement methods + add new ones */
  
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

}