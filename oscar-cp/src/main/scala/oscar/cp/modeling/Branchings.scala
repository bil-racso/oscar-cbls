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

package oscar.cp.modeling

import oscar.cp.core.CPVarInt
import oscar.cp.search._
import oscar.cp.scheduling.search.SetTimesBranching
import oscar.algo.search.BranchingUtils
import oscar.cp.core.CPVarSet

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
trait Branchings extends BranchingUtils {
  
  
  def binary(vars: Seq[_ <: CPVarInt], varHeuris: (CPVarInt => Int), valHeuris: (CPVarInt => Int) = minVal) = {
    new BinaryBranching(vars.toArray,varHeuris,valHeuris)
  }
 
  /**
   * Binary Search on the decision variables vars with fixed static ordering.
   * The next variable to assign is the first unbound variable in vars.
   * @param vars: the array of variables to assign during the search
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */  
  def binaryStatic(vars: Seq[_ <: CPVarInt], valHeuris: (CPVarInt => Int) = minVal) = new BinaryStaticOrderBranching(vars.toArray,valHeuris)

  /**
   * Binary First Fail (min dom size) on the decision variables vars.
   * @param vars: the array of variables to assign during the search
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */
  def binaryFirstFail(x: Seq[CPVarInt], valHeuris: (CPVarInt => Int) = minVal) = new BinaryFirstFailBranching(x.toArray, valHeuris)

  /**
   * Binary search on the decision variables vars, selecting first the variables having the max number of propagation methods attached to it.
   */
  def binaryMaxDegree(x: Seq[_ <: CPVarInt]) = new  BinaryMaxDegreeBranching(x.toArray)

  /**
   * Binary search on the decision variables vars, splitting the domain of the selected variable on the
   * median of the values (left : <= median, right : > median)
   */
  def binarySplit(x: Seq[CPVarInt], varHeuris: (CPVarInt => Int) = minVar, valHeuris: (Int => Int) = i => i) = new BinaryDomainSplitBranching(x.toArray, varHeuris, valHeuris)
  

  /**
   * set times heuristic: 
   * see: Time- versus-capacity compromises in project scheduling. (Le Pape et al.). 1994. 
   */  
  def setTimes(starts: IndexedSeq[CPVarInt], durations: IndexedSeq[CPVarInt], ends: IndexedSeq[CPVarInt]) = new SetTimesBranching(starts,durations, ends) 
  
  /**
   * Binary Search on the set variable
   * forcing an arbitrary on the left, and removing it on the right until the variable is bound
   */   
  def binary(x: CPVarSet) = {
    new BinarySetBranching(x)
  }
  
  

}