package oscar.lcg.search

import oscar.lcg.variables.IntVar
import oscar.lcg.core.Literal
import oscar.algo.reversible.ReversibleInt
import oscar.lcg.literals.LitLeq

class StaticHeuristic(variables: Array[IntVar], valHeuristic: Int => Int) extends Heuristic {

  require(variables.length > 0)
  
  private[this] val store = variables(0).store
  private[this] val nVariables = variables.length
  private[this] val depthRev = new ReversibleInt(store.trail, 0)
  private[this] var depth = 0

  final override def decision(): Literal = {
    // Cache
    depth = depthRev.value
    // Update depth 
    while (depth < nVariables && variables(depth).isAssigned) depth += 1
    if (depth == nVariables) null
    else {
      // Trail new depth
      depthRev.value = depth
      // Alternatives
      val variable = variables(depth)
      val value = valHeuristic(depth)
      new LitLeq(variable, value)
    }
  }
}