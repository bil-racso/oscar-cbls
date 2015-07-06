package oscar.cp.constraints

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPBoolVar
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.algo.reversible.ReversibleBoolean

final class LightBinaryKnapsack(items: Array[CPBoolVar], weights: Array[Int], load: CPIntVar) extends Constraint(load.store, "LightBinaryKnapsack") {

  private[this] val nItems = items.length
  private[this] val unassigned = Array.tabulate(nItems)(i => i)
  private[this] val nUnassignedRev = new ReversibleInt(s, nItems)
  private[this] var nUnassigned = 0

  private[this] val requiredLoadRev = new ReversibleInt(s, 0)
  private[this] val possibleLoadRev = new ReversibleInt(s, 0)
  private[this] var requiredLoad = 0
  private[this] var possibleLoad = 0

  final override def setup(l: CPPropagStrength): CPOutcome = {
    val outcome = init()
    if (outcome != Suspend) outcome
    else {
      var i = nItems
      while (i > 0) {
        i -= 1
        items(i).callPropagateWhenBind(this)
      }
      load.callPropagateWhenDomainChanges(this)
      propagate()//Suspend
    }
  }

  @inline private def init(): CPOutcome = {
    // Reset structures
    nUnassigned = nItems
    requiredLoad = 0
    possibleLoad = 0
    // Compute loads
    var i = nItems
    while (i > 0) {
      i -= 1
      val itemId = unassigned(i)
      val item = items(itemId)
      if (!item.isBound) possibleLoad += weights(i)
      else {
        // Remove from set
        nUnassigned -= 1
        unassigned(i) = unassigned(nUnassigned)
        unassigned(nUnassigned) = itemId
        // Update loads
        if (item.isTrue) {
          val weight = weights(itemId)
          requiredLoad += weight
          possibleLoad += weight
        }
      }
    }
    // Initial filtering
    if (filterItems == Failure) Failure
    else {
      nUnassignedRev.value = nUnassigned
      requiredLoadRev.value = requiredLoad
      possibleLoadRev.value = possibleLoad
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {
    // Cache
    nUnassigned = nUnassignedRev.value
    requiredLoad = requiredLoadRev.value
    possibleLoad = possibleLoadRev.value
    // Filtering
    updateAssignedItems()
    if (filterItems() == Failure) Failure
    else {
      // Trail
      nUnassignedRev.value = nUnassigned
      requiredLoadRev.value = requiredLoad
      possibleLoadRev.value = possibleLoad
      Suspend
    }
  }

  @inline private def updateAssignedItems(): Unit = {
    var i = nUnassigned
    while (i > 0) {
      i -= 1
      val itemId = unassigned(i)
      val item = items(itemId)
      if (items(itemId).isBound) {
        // Remove from set
        nUnassigned -= 1
        unassigned(i) = unassigned(nUnassigned)
        unassigned(nUnassigned) = itemId
        // Update loads
        if (item.isTrue) requiredLoad += weights(itemId)
        else possibleLoad -= weights(itemId)
      }
    }
  }

  @inline private def filterItems(): CPOutcome = {
    if (load.updateMax(possibleLoad) == Failure) Failure
    else if (load.updateMin(requiredLoad) == Failure) Failure
    else {
      val maxWeight = load.max - requiredLoad
      val minWeight = possibleLoad - load.min
      var i = nUnassigned
      while (i > 0) {
        i -= 1
        val itemId = unassigned(i)
        val item = items(itemId)
        val weight = weights(itemId)
        if (weight > maxWeight) {
          if (item.assignFalse() == Failure) return Failure
        } else if (minWeight < weight) {
          if (item.assignTrue() == Failure) return Failure
        }
      }
      Suspend
    }
  }
}