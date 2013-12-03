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
package oscar.cp.scheduling

import oscar.cp.core.CPVarInt

class ActivityPrecedence(v : CPVarInt, d : Int, exactly : Boolean) {
	
	private val cp = v.store

	def beforeEndOf(act : Activity)   = if (exactly) cp.add(v + d == act.end) else cp.add(v + d <= act.end)
	def beforeStartOf(act : Activity) = if (exactly) cp.add(v + d == act.start) else cp.add(v + d <= act.start)
}

object ActivityPrecedence {

	def apply(v : CPVarInt, i : Int, b : Boolean = false) = new ActivityPrecedence(v, i, b)
}
