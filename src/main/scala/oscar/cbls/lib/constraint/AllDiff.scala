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
/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.lib.constraint

import oscar.cbls._
import oscar.cbls.core._
import language.postfixOps

import scala.collection.immutable.SortedMap

/**
 * Implement the AllDiff constraint on IntVars: all variables must have a different value.
 * in ase the same variable occurs several times (in case you submit a list of CBLSIntVar) these two occurrences
 * will be supposed to have different values as expected by the semantics of this constraint
 * @param variables the variable whose values should all be different.
 * @author renaud.delandtsheer@cetic.be
 */
case class AllDiff(variables: Iterable[IntValue])
  extends Invariant with Constraint with IntNotificationTarget{
  //TODO: use bulking here

  val n = variables.size

  registerStaticAndDynamicDependencyAllNoID(variables)
  registerConstrainedVariables(variables)
  finishInitialization()
  val (minValueOfVars,maxValueOfVars) = InvariantHelper.getMinMaxBounds(variables)

  //le degré global de violation est la somme des tailles -1 des ensembles de var ayant meme value
  // et on ne prend que les ensembles de cardinalite > 1
  private val violationVariable: CBLSIntVar = new CBLSIntVar(model, 0, 0 to n, "ViolationsOfAllDiff")
  violationVariable.setDefiningInvariant(this)

  private val N0: Int = maxValueOfVars

  private val offset: Int = -minValueOfVars

  private val N = N0 + offset
  private val range = 0 to N

  private val valueMinusOffsetToNbOccurrence: Array[CBLSIntVar] = Array.tabulate[CBLSIntVar](N + 1)((i: Int) => {
    val tmp = new CBLSIntVar(model,0, 0 to n, "alldiff_count_of_value_" + (i - offset))
    tmp.setDefiningInvariant(this)
    tmp
  })

  for (v <- variables) {
    valueMinusOffsetToNbOccurrence(v.value + offset) :+= 1
  }

  for (i <- range) {
    val tmp = valueMinusOffsetToNbOccurrence(i).newValue - 1
    if (tmp > 0) violationVariable :+= tmp
  }

  /**the degree of violation of a variable is the number of other variables that have the same value as it. */
  private val Violations: SortedMap[IntValue, IntValue] = {
    def accumulate(acc:SortedMap[IntValue,IntValue], variable:IntValue, violation:IntValue):SortedMap[IntValue,IntValue] =
      acc + (acc.get(variable) match{
        case Some(oldViolation) => (variable,violation + oldViolation)
        case None => (variable,violation)})

    variables.foldLeft(
      SortedMap.empty[IntValue, IntValue])(
        (acc, intvar) => {
          val newvar = valueMinusOffsetToNbOccurrence.element(intvar + offset) - 1
          accumulate(acc , intvar, newvar)
        })
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, id:Int, OldVal: Int, NewVal: Int) {
    valueMinusOffsetToNbOccurrence(OldVal + offset) :-= 1
    valueMinusOffsetToNbOccurrence(NewVal + offset) :+= 1

    val deltaOldVal = if (valueMinusOffsetToNbOccurrence(OldVal + offset).newValue == 0) 0 else -1
    val deltaNewVal = if (valueMinusOffsetToNbOccurrence(NewVal + offset).newValue == 1) 0 else 1
    violationVariable :+= (deltaNewVal + deltaOldVal)
  }

  /**
   * The degree of violation of this constraint is the number of variables that should be changed
   * to ensure that the constraint is not violated.
   * @return an IntVar that can be incorporated in an invariant.
   */
  override def violation = violationVariable

  /**
   * The degree of violation of a variable is the number of other variables that have the same value
   * @return an IntVar that can be incorporated in an invariant.
   */
  override def violation(v: Value): IntValue = {
    val tmp: IntValue = Violations.getOrElse(v.asInstanceOf[IntValue], null)
    assert(tmp != null)
    tmp
  }

  override def checkInternals(c: Checker) {
    val myValueCount: Array[Int] = (for (i <- 0 to N) yield 0).toArray
    for (v <- variables) myValueCount(v.value + offset) += 1
    for (v <- range) {
      c.check(valueMinusOffsetToNbOccurrence(v).newValue == myValueCount(v),
        Some("valueMinusOffsetToNbOccurrence(" + v + ").newValue (" + valueMinusOffsetToNbOccurrence(v).newValue
          + ") == myValueCount(" + v + ") (" + myValueCount(v)))
    }

    for (v <- variables) {
      c.check(violation(v).value == myValueCount(v.value + offset) - 1,
        Some("violation(" + v.name + ").value (" + violation(v).value
          + ") != myValueCount(" + v.name + ".value + offset) - 1 ("
          + (myValueCount(v.value + offset) - 1) + ")"))
    }

    var MyViol: Int = 0
    for (v <- range) MyViol += (0.max(myValueCount(v) - 1))
    c.check(MyViol == violationVariable.value, Some("MyViol (" + MyViol
        + ") == violationVariable.value (" + violationVariable.value + ")"))
  }
}
