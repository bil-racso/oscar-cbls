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
package oscar.examples.userman.cp

import oscar.cp.modeling._
import oscar.cp.scheduling._

object FirstScheduling extends App{
	
	val horizon = 8
	val cp = CPScheduler(horizon)
	
	val act1 = Activity(cp, 2, "My first activity")
	val act2 = Activity(cp, 4, "My second activity")
	
	cp.solve subjectTo {	
		act1 precedes act2
		
	} exploration {
		cp.binary(cp.activities.map(_.start))
	}
	
	println(act1.name + " starts at " + act1.start)
	println(act2.name + " starts at " + act2.start)
}
