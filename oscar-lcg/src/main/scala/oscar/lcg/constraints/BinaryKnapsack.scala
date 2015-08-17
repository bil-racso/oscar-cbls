package oscar.lcg.constraints

import oscar.lcg.core.Constraint
import oscar.lcg.variables.BooleanVar
import oscar.lcg.variables.IntVar
import oscar.algo.reversible.ReversibleInt
import oscar.lcg.core.Literal
import oscar.lcg.literals.LitFalse

final class BinaryKnapsack(items: Array[BooleanVar], costs: Array[Int], cost: IntVar, override val name: String) extends Constraint {

  require(items.length > 0)
  
  private[this] val trail = items(0).store.trail
  private[this] val nItems = items.length
  
  private[this] val explanation = new Array[Literal](nItems)
  
  private[this] val unassigned = Array.tabulate(nItems)(i => i)
  private[this] val nUnassignedRev = new ReversibleInt(trail, nItems)
  private[this] var nUnassigned = nItems
  
  private[this] val possibleCostRev = new ReversibleInt(trail, sumCost)
  private[this] val requiredCostRev = new ReversibleInt(trail, 0)
  private[this] var possibleCost = 0
  private[this] var requiredCost = 0
  
  override def setup(): Boolean = {
    if (!filter) false
    else {
      var i = nUnassigned
      while (i > 0) { i -= 1; items(unassigned(i)).awakeOnAssign(this) }
      cost.awakeOnChanges(this)
      true
    }
  } 
  
  override def filter(): Boolean = {
    cache()
    updateAssigned()
    if (!filterItems()) false
    else { trail(); true }
  }
  
  @inline private def updateAssigned(): Unit = {
    var i = nUnassigned
    possibleCost = possibleCost
    while (i > 0) {
      i -= 1
      val varId = unassigned(i)
      val item = items(varId)
      if (item.isAssigned) {
        nUnassigned -= 1
        unassigned(i) = unassigned(nUnassigned)
        unassigned(nUnassigned) = varId
        if (item.isTrue) requiredCost += costs(varId)
        else possibleCost -= costs(varId)
      } 
    }
  }
  
  @inline def uglyHack(explanationSize: Int): Boolean = {
    LitFalse.explain(explanation, explanationSize)
    items(0).store.failedLiteral = LitFalse
    items(0).store.explained(LitFalse)
    false
  }
  
  @inline private def filterItems(): Boolean = {
    val explanationSize = fillExplanation()
    if (!cost.updateMin(requiredCost, Array.empty)) uglyHack(explanationSize)
    else if (!cost.updateMax(possibleCost, Array.empty)) uglyHack(explanationSize)
    else {
      val maxCost = cost.max - requiredCost
      val minCost = possibleCost - cost.min
      var i = nUnassigned
      while (i > 0) {
        i -= 1
        val varId = unassigned(i)
        val cost = costs(varId)
        val item = items(varId)
        if (cost > maxCost) {
          if (!item.assignFalse(explanation, explanationSize)) return false
        } else if (cost > minCost) {
          if (!item.assignTrue(explanation, explanationSize)) return false          
        }
      }
      true
    }
  }
  
  @inline private def cache(): Unit = {
    possibleCost = possibleCostRev.value 
    requiredCost = requiredCostRev.value
    nUnassigned = nUnassignedRev.value
  }
  
  @inline private def trail(): Unit = {
    possibleCostRev.value = possibleCost
    requiredCostRev.value = requiredCost
    nUnassignedRev.value = nUnassigned
  }
  
  @inline private def sumCost: Int = {
    var i = nItems
    var s = 0
    while (i > 0) { i -= 1; s += costs(i) }
    s
  }
  
  @inline def fillExplanation(): Int = {
    var i = nUnassigned
    while (i < nItems) {
      explanation(i - nUnassigned) = items(unassigned(i)).eqLit
      i += 1
    }
    nItems - nUnassigned
  }
}