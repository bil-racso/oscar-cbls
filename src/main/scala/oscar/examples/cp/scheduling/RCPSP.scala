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
/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.examples.cp.scheduling

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.scheduling._
import oscar.cp.constraints._
import oscar.cp.core.CPVarInt

/**
 *
 *  @authors: Pierre Schaus  pschaus@gmail.com
 *  @authors: Renaud Hartert ren.hartert@gmail.com
 */
object RCPSP extends App {

	// (duration, consumption)
	val instance = Array((5, 1), (3, 1), (9, 3), (1, 2), (2, 2), (8, 1), (3, 2), (2, 2), (2, 1), (1, 1), (1, 2))
	val capa = 4

	val horizon = instance.map(_._1).sum
	val cp = CPScheduler(horizon)

	val resource   = MaxResource(cp, capa)
	val activities = instance.map(a => Activity(cp, a._1))

	val makespan = cp.makespan
	
	cp.minimize(makespan) subjectTo {

		for (a <- 0 until instance.size)
			activities(a) needs instance(a)._2 ofResource resource

	} exploration {

		cp.setTimes(cp.activities)
	} run()
	cp.printStats()
}
