package oscar.lcg.modeling

import oscar.lcg.searches.Heuristic
import oscar.lcg.searches.StaticHeuristic
import oscar.lcg.variables.BooleanVar
import oscar.lcg.variables.IntVar
import oscar.lcg.searches.StaticHeuristicBoolean

trait Heuristics {

  final def static(variables: Array[IntVar], valHeuristic: Int => Int): Heuristic = {
    new StaticHeuristic(variables, valHeuristic)
  }
  
  final def static(variables: Array[IntVar]): Heuristic = {
    new StaticHeuristic(variables, variables(_).min)
  }
  
  final def static(variables: Array[BooleanVar], valHeuristic: Int => Boolean): Heuristic = {
    new StaticHeuristicBoolean(variables, valHeuristic)
  }
  
  final def static(variables: Array[BooleanVar]): Heuristic = {
    new StaticHeuristicBoolean(variables, i => true)
  }
}