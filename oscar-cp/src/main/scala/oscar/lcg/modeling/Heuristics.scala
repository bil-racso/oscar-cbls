package oscar.lcg.modeling

import oscar.lcg.heuristic.StaticMinHeuristic
import oscar.lcg.heuristic.Heuristic
import oscar.lcg.variables.LCGIntervalVar
import oscar.lcg.heuristic.MinMinHeuristic

/** @author Renaud Hartert ren.hartert@gmail.com */
trait Heuristics {

  final def static(variables: Array[LCGIntervalVar]): Heuristic = new StaticMinHeuristic(variables)
  
  final def minValue(variables: Array[LCGIntervalVar]): Heuristic = new MinMinHeuristic(variables)
  
}