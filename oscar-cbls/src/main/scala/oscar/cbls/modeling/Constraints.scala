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
package oscar.cbls.modeling

import oscar.cbls.invariants.core.computation.CBLSIntVar
import collection.immutable.SortedMap
import oscar.cbls.constraints.lib.global.{MultiKnapsack, AtMost, AtLeast, AllDiff, Sequence}

trait Constraints {

  /**Implement the AllDiff constraint on IntVars: all variables must have a different value.
   * @param variables the variable whose values should all be different.
   */
  def allDifferent(variables:Iterable[CBLSIntVar]) = AllDiff(variables)


  /**Implement the AtLeast constraint on IntVars.
   * There is a set of minbounds, defined in the parameter bound as pair (value,minbound).
   * The variables should be such that there is at least ''minbound'' of them which have the value ''value''.
   *
   * @param variables the variable whose values are constrained
   * @param bounds map(value,minbound) specifying the minimal number of occurrence of ''value'' among the variables.
   * We use a map to ensure that there is no two bounds on the same value.
   */
  def atLeast(variables:Iterable[CBLSIntVar], bounds:SortedMap[Int, CBLSIntVar]) = AtLeast(variables, bounds)


  /**Implements the AtMost constraint on IntVar.
   * There is a set of bounds, defined in the parameter bound as pair (value,bound).
   * The variables should be such that there is at most ''bound'' of them which have the value ''value''.
   * WARNING: not tested!
   * @param variables the variables that should be bounded
   * @param bounds map(value,bound) the bounds on the variables. We use a map to ensure that there is no two bounds on the same value.
   */
  def atMost(variables:Iterable[CBLSIntVar], bounds:SortedMap[Int, Int]) = AtMost(variables, bounds)


  /**This is the standard bin packing constraint
   * @param items the items, designing the bins they are placed into
   * @param itemsizes the size of the items
   * @param binsizes the max size of the available bins
   */
  def multiKnapsack(items: Array[CBLSIntVar], itemsizes: Array[CBLSIntVar], binsizes:Array[CBLSIntVar]) = MultiKnapsack(items, itemsizes, binsizes)


  /**implements the sequence constraint:
   *
   * @param variables the "history variables"
   * @param length the length of the sequence
   * @param Max the max number of elements matching pred in all sequences of the history
   * @param predicate a predicate to say which values belong to the constraint
   */
  def sequence(variables: Array[CBLSIntVar], length:Int, Max:Int, predicate:(Int=>Boolean)) = Sequence(variables, length, Max, predicate)

}
