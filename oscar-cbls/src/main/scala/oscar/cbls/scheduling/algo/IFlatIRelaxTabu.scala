package oscar.cbls.scheduling.algo

/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

import oscar.cbls.invariants.core.computation.{ CBLSIntVar, Solution, Store }
import oscar.cbls.scheduling.model._

/**
 * @param p
 * @param verbose
 * @author renaud.delandtsheer@cetic.be
 */
class IFlatIRelaxTabu(p: Planning,
                      verbose: Boolean = true,
                      nbRelax: Int = 4,
                      pkillPerRelax: Int = 50,
                      tenure:Int = 10) extends IFlatIRelax(p,verbose, nbRelax, pkillPerRelax) {

  val initialTabu = 0

  //the iteration where the precedence from-to has been killed last
  val tabu: Array[Array[Int]] = Array.fill(p.activityCount, p.activityCount)(initialTabu)


  override def doRelax(from: Activity, to: Activity, verbose: Boolean){
    tabu(from.ID)(to.ID) = scala.math.max(it, tabu(from.ID)(to.ID)) + tenure
    super.doRelax(from, to, verbose)
  }

  override def estimateMakespanExpansionForNewDependency(from: Activity, to: Activity): Int ={
    super.estimateMakespanExpansionForNewDependency(from, to) + scala.math.max(0,tabu(from.ID)(to.ID) - it)
  }
}
