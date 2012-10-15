/** *****************************************************************************
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

package oscar.search

import oscar.reversible._
import scala.collection.mutable.Stack
import scala.util.continuations._

/** Controller for iterative discrepancy search
  * @author Pierre Schaus pschaus@gmail.com & Sebastien Mouthuy smouthuy@gmail.com
  */
class IDSSearchController(node: ReversibleSearchNode, val maxDiscrepency: Int) extends SearchController(node) {

	val stack: Stack[MyContinuation] = new Stack()
	val discr = new ReversibleInt(node, 0)
	val inDFS = new ReversibleBool(node, false)
	var maxDiscr = 10

	def addChoice(e: MyContinuation) {
		node.pushState()
		stack.push(e)
	}

	override def reset() {
		super.reset()
		stack.clear()
	}

	override def start() = {
		discr.setValue(0)
		node.branchAll(0 to maxDiscrepency) { i =>
			discr.setValue(0)
			maxDiscr = i // max discrepancy of this DFS search = i
			println("discrepency = " + i)
		}
		inDFS.setValue(true)
	}

	override def fail() {
		super.fail()
		discr.incr() // increment the number of discrepancy
		if (inDFS.value && discr.value > maxDiscr) {
			node.fail()
		}
	}

	def explore() {
		while (!stack.isEmpty && !limitReached) {
			node.pop()
			fail()
			stack.pop.call()
		}
	}

}

