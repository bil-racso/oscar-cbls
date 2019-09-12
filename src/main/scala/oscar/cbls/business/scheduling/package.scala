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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/
package oscar.cbls.business

import oscar.cbls.Store
import oscar.cbls.business.scheduling.model.ResourceConstraint
import oscar.cbls.business.scheduling.modeling.SchedulingInvariants

package object scheduling extends SchedulingInvariants {
  type Schedule = oscar.cbls.business.scheduling.model.Schedule

  def schedule(m: Store, durations: Array[Long], precPairs: List[(Int, Int)], minStartTimes: Map[Int, Long],
               initialActs: Iterable[Int], resConstraints: Array[ResourceConstraint]): Schedule =
    new Schedule(m, durations, precPairs, minStartTimes, initialActs, resConstraints)
}
/*
package oscar.cbls.business

/**
 * DEPECATED: the whole scheduling package is deprecated since OScaR 4.0 and will be fully reworked
 *  This package is a scheduling library.
 * it supports
 - [[oscar.cbls.business.scheduling.model.CumulativeResource]]
 - [[oscar.cbls.business.scheduling.model.Activity]] with varying durations and precedence constraints
 - [[oscar.cbls.business.scheduling.model.SuperActivity]] that align their start and end to other tasks.
 *   This is useful to model that a resource is not released between tasks.
 *
 * In this package, Tasks are grouped into [[oscar.cbls.business.scheduling.model.Planning]] that keeps references to all tasks and resources.
 * This package features the [[oscar.cbls.business.scheduling.solver.IFlatIRelax]] search heuristics with various tunings
 * @author renaud.delandtsheer@cetic.be
 * */
package object scheduling {

}
*/