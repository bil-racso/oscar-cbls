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


package oscar.cbls.constraints.lib.global

import collection.immutable.SortedMap
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.core.computation._
;

/**implments the sequence constraint:
 *
 * @param variables the "history variables"
 * @param length the length of the sequence
 * @param Max the max number of elements matchind pred in all sequences of the history
 * @param predicate a predicate to say which values belong to the constraint
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class Sequence(variables: Array[IntVar], length:Int, Max:Int, predicate:(Int=>Boolean))
  extends Constraint {

  registerStaticAndDynamicDependencyArrayIndex(variables)
  registerConstrainedVariables(variables)

  finishInitialization()

  val count = (for(i <- 0 until variables.length - length) yield IntVar(model,0, length, 0 ,"sequence_count_" + i)).toArray
  val violated = (for(i <- 0 until variables.length - length) yield IntVar(model,0, length - Max, 0 ,"is_violated_sequence" + i)).toArray
  var Violations = SortedMap.empty[Variable, IntVar]

  for(i <- 0 until variables.length - length){
    val violatedvars = for(j <- i until min(variables.length,i + length)) yield violated(j)
    Violations = Violations + ((variables(i),Sum(violatedvars).toIntVar))
  }

  val Violation = IntVar(model,0, variables.length * length, 0 ,"sequence_violations")

  for(i <- variables.indices){
    if(predicate(variables(i).value)){
      for(j <- i until min(variables.length-1,i+length)){
        count(j) :+= 1
        if(count(j).getValue(true) > Max){
          violated(j) :+=1
          Violation :+= 1
        }
      }
    }
  }

  @inline
  override def notifyIntChanged(v: IntVar, i: Int, OldVal: Int, NewVal: Int){
    if (predicate(OldVal)){
      if(!predicate(NewVal)){
        //decrease the count
        for(j <- i until min(variables.length-1,i+length)){
          count(j) :-= 1
          if(count(j).getValue(true) >= Max){
            violated(j) :-=1
            Violation :-= 1
          }
        }
      }
    }else{
      if(predicate(variables(i).value)){
        //increase the count
        for(j <- i until min(variables.length-1,i+length)){
          count(j) :+= 1
          if(count(j).getValue(true) > Max){
            violated(j) :+=1
            Violation :+= 1
          }
        }
      }
    }
  }

  private def min(a:Int, b:Int):Int = if (a>b) b else a

  def violation(v: Variable): IntVar = Violations.getOrElse(v,null)

  def violation: IntVar = Violation
}
