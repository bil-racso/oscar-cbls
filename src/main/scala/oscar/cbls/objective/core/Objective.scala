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
 ******************************************************************************/
/*
 * Copyright CETIC 2012 www.cetic.be
 *
 * This file is part of Asteroid.
 *
 * Asteroid is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Asteroid is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asteroid.
 * If not, see http://www.gnu.org/licenses/lgpl-2.1-standalone.html
 *
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 */

package oscar.cbls.objective.core

import collection.immutable.SortedMap;
import oscar.cbls.invariants.core.computation._;

import oscar.cbls.invariants.core.computation.IntVar._

case class Objective(ObjectiveVar:IntVar) extends ObjectiveTrait {
  setObjectiveVar(ObjectiveVar)
}

trait ObjectiveTrait{

  private var ObjectiveVar:IntVar = null

  def setObjectiveVar(v:IntVar){
    ObjectiveVar = v
    ObjectiveVar.getPropagationStructure.registerForPartialPropagation(ObjectiveVar)
  }

  /**returns the value of the objective variable if the two variables a and b were swapped values.
   * This proceeds through explicit state change and restore.
   * this process is efficiently performed as the objective Varaible is registered for partial propagation
   * @see registerForPartialPropagation() in [[invariants.core.computation.Model]]
   */
  def getSwapVal(a:IntVar,b:IntVar):Int = {
    a :=: b
    val NewVal:Int = ObjectiveVar
    a :=: b
    NewVal
  }

  /**returns the value of the objective variable if variable a was assigned the value v.
   * This proceeds through explicit state change and restore.
   * this process is efficiently performed as the objective Varaible is registered for partial propagation
   * @see registerForPartialPropagation() in [[invariants.core.computation.Model]]
   */
  def getAssignVal(a:IntVar,v:Int):Int = getAssignVal(SortedMap(a -> v))

  /**returns the value of the objective variable if the assignment described by parameter a was performed
   * This proceeds through explicit state change and restore.
   * this process is efficiently performed as the objective Varaible is registered for partial propagation
   * @see registerForPartialPropagation() in [[invariants.core.computation.Model]]
   */
  def getAssignVal(a:SortedMap[IntVar,Int]):Int = {
    //memorize
    val oldvals:Map[IntVar,Int] = a.foldLeft(SortedMap.empty[IntVar,Int])(
      (acc,IntVarAndInt) => acc + ((IntVarAndInt._1,IntVarAndInt._1.getValue())))

    //excurse
    for(assign <- a){assign._1 := assign._2}
    val NewVal:Int = ObjectiveVar

    //undo
    for(assign <- oldvals ){assign._1 := assign._2}

    NewVal
  }
}
