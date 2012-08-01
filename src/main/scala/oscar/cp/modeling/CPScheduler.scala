package oscar.cp.modeling

import oscar.cp.scheduling._
import oscar.reversible.ReversibleBool
import oscar.reversible.ReversibleInt

import scala.util.continuations._
import scala.collection.JavaConverters._
import scala.collection.mutable.Set

class CPScheduler(val horizon : Int) extends CPSolver {

	private var nResource = 0
	private val resources : Set[Resource] = Set()

	def addResource(resource : Resource) : Int = {

		resources.add(resource)
		nResource += 1

		return nResource
	}

	override def subjectTo(constraintsBlock : => Unit) : CPSolver = {
		
		try {
			constraintsBlock
		} catch {
			case ex : NoSol => println("No Solution, inconsistent model")
		}
		
		try {
			for (r <- resources) r.setup
		} catch {
			case ex : NoSol => println("No Solution, inconsistent model (resource constraints)")
		}

		this
	}

	def setTimesSearch(activities : Array[Activity]) : Unit @suspendable = {

		// Non fixed activities
		val selectable = Array.tabulate(activities.size) {
			i =>
				if (activities(i).start.isBound)
					new ReversibleBool(this, false)
				else
					new ReversibleBool(this, true)
		}

		val oldEST = Array.fill(activities.size)(new ReversibleInt(this, -1))

			def updateSelectable() = {

				for (i <- 0 until activities.size) {
					if (activities(i).start.isBound) {
						selectable(i).value = false

					} else if (oldEST(i).value != activities(i).est) {
						selectable(i).value = true
					}
				}
			}

			def selectableIndices() = (0 until activities.size).filter(i => selectable(i).value)

			def allStartBounds() = activities.forall(i => i.start.isBound)

		while (!allStartBounds()) {

			// Get the smallest EST
			val (est, ect) = selectableIndices().map(i => (activities(i).est, activities(i).ect)).min

			// Select the activity with the smallest EST, ECT as tie breaker
			val x = selectableIndices().filter(i => activities(i).est == est && activities(i).ect == ect).first

			branch {

				post(activities(x).start == est)
				oldEST(x).value = -1
				updateSelectable()

				if (selectableIndices().isEmpty && !allStartBounds()) fail()
			} {

				selectable(x).value = false
				oldEST(x).value = est
				updateSelectable()

				if (selectableIndices().isEmpty && !allStartBounds()) fail()
			}
		}
	}
}