/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.constraints.lib.basic

import oscar.cbls.invariants.lib.numeric.Implicits._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.constraints.core._
import oscar.cbls.invariants.lib.numeric.Minus._
import oscar.cbls.invariants.lib.numeric.{Abs, Minus}

/**
 * implements left <= right
 */
case class LE(left:IntVar, right:IntVar) extends Constraint {

  registerConstrainedVariables(left,right)

  val Violation:IntVar = Max2(0,left minus right)

  finishInitialization()

  /**the violation is Max(0,right-left)
   */
  override def getViolation = Violation

  /**The violation of each variable is equal to the global violation of the constraint
   */
  override def getViolation(v: Variable):IntVar = {if(left==v || right==v) Violation else 0}
}

/**
 * implements left >= right
 * it is just a parameter swap of [[constraints.lib.basic.LE]]
 */
case class GE(override val left:IntVar, override val right:IntVar) extends LE(right,left)

/**
 * implements left < right
 * it is just the 
 */
case class L(left:IntVar, right:IntVar) extends Constraint{
  registerConstrainedVariables(left,right)

  val Violation:IntVar = Max2(0, left minus right plus 1)
  finishInitialization(Violation.getPropagationStructure)

  /**the violation is Max(0,right-left + 1)
   */
  override def getViolation = Violation
  /**The violation of each variable is equal to the global violation of the constraint
   */
  override def getViolation(v: Variable):IntVar = {if(left==v || right==v) Violation else 0}
}

/**
 * implements left > right
 * it is just a parameter swap of [[constraints.lib.basic.L]]
 */
case class G(override val left:IntVar, override val right:IntVar) extends L(right,left)

/**
 * implements left != right
 */
case class NE(left:IntVar, right:IntVar) extends Constraint{
  registerConstrainedVariables(left,right)
  registerStaticAndDynamicDependenciesNoID(left,right)
  finishInitialization()

  val Violation:IntVar = new IntVar(model,0,1,1,"equals")

  Violation.setDefiningInvariant(this)

  @inline
  override def notifyIntChanged(v:IntVar,OldVal:Int,NewVal:Int){
    Violation := (if(left.getValue() == right.getValue()) 1 else 0)
  }

  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override def getViolation = Violation
  /** the violation is 1 if the variables are equal, 0 otherwise*/
  override def getViolation(v: Variable):IntVar = {if(left==v || right==v) Violation else 0}
}

/**constraints left == right
 * this is considered as a primitive constraint and used in the [[constraints.core.Constraint]]
 * class, so that it is part of the core instead of the library*/
case class EQ(left:IntVar, right:IntVar) extends Constraint{

  registerConstrainedVariables(left,right)
  finishInitialization()

  //todo: supprimer ces invariants
  val Violation:IntVar = Abs(Minus(left,right))

  override def getViolation:IntVar = Violation
  override def getViolation(v: Variable):IntVar = {if(left==v || right==v) Violation else 0}
}
