package oscar.cbls.modeling

import oscar.cbls.invariants.core.computation.IntVar
import collection.immutable.SortedMap
import oscar.cbls.constraints.lib.global.{MultiKnapsack, AtMost, AtLeast, AllDiff, Sequence}

trait Constraints {

  /**Implement the AllDiff constraint on IntVars: all variables must have a different value.
   * @param variables the variable whose values should all be different.
   */
  def alldifferent(variables:Iterable[IntVar]) = AllDiff(variables)


  /**Implement the AtLeast constraint on IntVars.
   * There is a set of minbounds, defined in the parameter bound as pair (value,minbound).
   * The variables should be such that there is at least ''minbound'' of them which have the value ''value''.
   *
   * @param variables the variable whose values are constrained
   * @param bounds map(value,minbound) specifying the minimal number of occurrence of ''value'' among the variables.
   * We use a map to ensure that there is no two bounds on the same value.
   */
  def atleast(variables:Iterable[IntVar], bounds:SortedMap[Int, IntVar]) = AtLeast(variables, bounds)


  /**Implements the AtMost constraint on IntVar.
   * There is a set of bounds, defined in the parameter bound as pair (value,bound).
   * The variables should be such that there is at most ''bound'' of them which have the value ''value''.
   * WARNING: not tested!
   * @param variables the variables that should be bounded
   * @param bounds map(value,bound) the bounds on the variables. We use a map to ensure that there is no two bounds on the same value.
   */
  def atmost(variables:Iterable[IntVar], bounds:SortedMap[Int, Int]) = AtMost(variables, bounds)


  /**This is the standard bin packing constraint
   * @param items the items, designing the bins they are placed into
   * @param itemsizes the size of the items
   * @param binsizes the max size of the available bins
   */
  def multiknapsack(items: Array[IntVar], itemsizes: Array[IntVar], binsizes:Array[IntVar]) = MultiKnapsack(items, itemsizes, binsizes)


  /**implments the sequence constraint:
   *
   * @param variables the "history variables"
   * @param length the length of the sequence
   * @param Max the max number of elements matchind pred in all sequences of the history
   * @param predicate a predicate to say which values belong to the constraint
   */
  def sequence(variables: Array[IntVar], length:Int, Max:Int, predicate:(Int=>Boolean)) = Sequence(variables, length, Max, predicate)

}