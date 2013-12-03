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


package oscar.cbls.constraints.lib.basic

import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.constraints.core._
import oscar.cbls.invariants.lib.numeric.{Abs, Minus}


/**
 * implements left <= right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
protected class LEA(val left:IntVar, val right:IntVar) extends Constraint {

  registerConstrainedVariables(left,right)

  val Violation:IntVar = Max2(0,left - right)

  finishInitialization()

  /**the violation is Max(0,right-left)
   */
  override def violation = Violation

  /**The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Variable):IntVar = {if(left==v || right==v) Violation else 0}
}

/**
 * implements left <= right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class LE(l:IntVar,r:IntVar) extends LEA(l,r)

/**
 * implements left >= right
 * it is just a parameter swap of [[oscar.cbls.constraints.lib.basic.LE]]
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class GE(l:IntVar,r:IntVar) extends LEA(r,l)

/**
 * implements left < right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
protected class LA(val left:IntVar, val right:IntVar) extends Constraint{
  registerConstrainedVariables(left,right)

  val Violation:IntVar = Max2(0, left + right + 1)
  finishInitialization(Violation.getPropagationStructure)

  /**the violation is Max(0,right-left + 1)
   */
  override def violation = Violation
  /**The violation of each variable is equal to the global violation of the constraint
   */
  override def violation(v: Variable):IntVar = {if(left==v || right==v) Violation else 0}
}

/**
 * implements left > right
 * it is just a parameter swap of [[oscar.cbls.constraints.lib.basic.L]]
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class L(l:IntVar,r:IntVar) extends LA(l,r)

/**
 * implements left < right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class G(l:IntVar,r:IntVar) extends LA(r,l)

/**
 * implements left != right
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class NE(left:IntVar, right:IntVar) extends Constraint{
  registerConstrainedVariables(left,right)
  registerStaticAndDynamicDependenciesNoID(left,right)
  finishInitialization()

  val Violation:IntVar = new IntVar(model,0,1,1,"equals")

  Violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    Violation := (if(left.value == right.value) 1 else 0)
  }

  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override def violation = Violation
  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override def violation(v: Variable):IntVar = {if(left==v || right==v) Violation else 0}
}

/**constraints left == right
 * this is considered as a primitive constraint and used in the [[oscar.cbls.constraints.core.Constraint]]
 * class, so that it is part of the core instead of the library
 * @author  Renaud De Landtsheer rdl@cetic.be
 */
case class EQ(left:IntVar, right:IntVar) extends Constraint{

  registerConstrainedVariables(left,right)
  finishInitialization()

  val Violation:IntVar = Abs(Minus(left,right))

  override def violation:IntVar = Violation
  override def violation(v: Variable):IntVar = {if(left==v || right==v) Violation else 0}
}
